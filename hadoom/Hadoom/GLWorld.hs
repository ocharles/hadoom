{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

-- | This module allows a 'Hadoom.World.World' to be compiled into a description
-- that can be viewed via OpenGL.
module Hadoom.GLWorld where

import Control.Applicative
import Control.Lens hiding (indices)
import Control.Monad
import Control.Monad.Fix
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.TList
import Foreign
import Foreign.C.Types
import Graphics.GL
import Hadoom.World
import Linear
import Material
import Shader
import Util
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Sector
import qualified Hadoom.GL.Vertex as GL

data GLInterpretation :: SceneElementType -> * where
  GLVertex :: V2 Float -> GLInterpretation TVertex
  GLWall :: IO () -> GLInterpretation TWall
  GLSector :: SectorProperties -> IO () -> Material.Material -> GLInterpretation TSector
  GLWorld :: [GLInterpretation TSector] -> [GLInterpretation TWall] -> GLInterpretation TWorld
  GLTexture :: GLTextureObject -> GLInterpretation TTexture
  GLMaterial :: Material.Material -> GLInterpretation TMaterial

drawWorldTextured :: GLInterpretation TWorld -> IO ()
drawWorldTextured (GLWorld sectors walls) =
  do traverse_ (\(GLSector _ io mat) ->
                  do Material.activateMaterial mat
                     io)
               sectors
     traverse_ (\(GLWall io) -> io) walls

drawWorldGeometry :: GLInterpretation TWorld -> IO ()
drawWorldGeometry (GLWorld sectors walls) =
  do traverse_ (\(GLSector _ io _) -> io) sectors
     traverse_ (\(GLWall io) -> io) walls

compile :: PWorld l -> IO (GLInterpretation l)
compile (PWorld levelExpr) = go levelExpr
  where go :: WorldExpr GLInterpretation t -> IO (GLInterpretation t)
        go (Let bindings in_) =
          do bindings' <- mfix (ttraverse go . bindings)
             go (in_ bindings')
        go (Var x) = return x
        go (Vertex v) = return (GLVertex v)
        go (Texture path) =
          GLTexture <$>
          loadTexture path SRGB
        go (Hadoom.World.Material mkDiffuse mkNormals) =
          do GLMaterial <$>
               (Material.Material <$>
                ((\(GLTexture t) -> t) <$>
                 go mkDiffuse) <*>
                ((\(Just (GLTexture t)) -> t) <$>
                 traverse go mkNormals))
        go (World mkSectors mkWalls) =
          do sectors <- traverse go mkSectors
             walls <- traverse go mkWalls
             return (GLWorld sectors walls)
        go (Sector props mkVertices mkMat _ _) =
          do
             -- Realise all vertices
             vertices <- map (\(GLVertex v) -> v) <$>
                         traverse go mkVertices
             let floorVertices =
                   vertices <&>
                   \(V2 x y) ->
                     GL.Vertex
                       (realToFrac <$>
                        V3 x (sectorFloor props) y)
                       (V3 0 1 0)
                       (V3 1 0 0)
                       (V3 0 0 (-1))
                       (realToFrac <$>
                        (V2 x y ^*
                         recip textureSize))
                 ceilingVertices =
                   floorVertices <&>
                   \(GL.Vertex p n t bn uv) ->
                     GL.Vertex
                       (p & _y .~
                        realToFrac (sectorCeiling props))
                       (negate n)
                       t
                       bn
                       uv
                 allVertices = floorVertices <> ceilingVertices
             -- Allocate a vertex array object for the floor and ceiling
             vao <- overPtr (glGenVertexArrays 1)
             glBindVertexArray vao
             -- Allocate a buffer for the vertex data.
             vbo <- overPtr (glGenBuffers 1)
             glBindBuffer GL_ARRAY_BUFFER vbo
             glBufferData
               GL_ARRAY_BUFFER
               (fromIntegral
                  (sizeOf (undefined :: GL.Vertex) *
                   length allVertices))
               nullPtr
               GL_STATIC_DRAW
             withArray allVertices
                       (glBufferSubData
                          GL_ARRAY_BUFFER
                          0
                          (fromIntegral
                             (sizeOf (undefined :: GL.Vertex) *
                              length allVertices)) .
                        castPtr)
             GL.configureVertexAttributes
             -- Build the element buffer
             let floorIndices :: V.Vector GLuint
                 floorIndices =
                   fromIntegral <$>
                   Sector.triangulate (V.fromList vertices)
                 ceilingIndices =
                   let reverseTriangles v =
                         case V.splitAt 3 v of
                           (h,t)
                             | V.length h == 3 ->
                               V.fromList [h V.! 0,h V.! 2,h V.! 1] V.++
                               reverseTriangles t
                           _ -> mempty
                   in V.map (+ (fromIntegral (length floorVertices)))
                            (reverseTriangles floorIndices)
                 indices = floorIndices <> ceilingIndices
                 iboSize =
                   fromIntegral
                     (sizeOf (0 :: GLuint) *
                      V.length indices)
             ibo <- overPtr (glGenBuffers 1)
             glBindBuffer GL_ELEMENT_ARRAY_BUFFER ibo
             glBufferData GL_ELEMENT_ARRAY_BUFFER iboSize nullPtr GL_STATIC_DRAW
             SV.unsafeWith
               (V.convert indices)
               (glBufferSubData GL_ELEMENT_ARRAY_BUFFER 0 iboSize .
                castPtr)
             GLMaterial mat <- go mkMat
             return (GLSector props
                              (do glBindVertexArray vao
                                  glDrawElements GL_TRIANGLES
                                                 (fromIntegral (V.length floorIndices))
                                                 GL_UNSIGNED_INT
                                                 nullPtr
                                  glDrawElements
                                    GL_TRIANGLES
                                    (fromIntegral (V.length ceilingIndices))
                                    GL_UNSIGNED_INT
                                    (nullPtr `plusPtr`
                                     fromIntegral
                                       (sizeOf (0 :: GLuint) *
                                        V.length floorIndices)))
                              mat)
        go (Wall mkV1 mkV2 mkFrontSector mkBackSector) =
          do
             -- Allocate a vertex array object for this wall.
             vao <- overPtr (glGenVertexArrays 1)
             glBindVertexArray vao
             -- Evaluate the start and end vertices, and the front and back sectors.
             GLVertex v1 <- go mkV1
             GLVertex v2 <- go mkV2
             GLSector (SectorProperties frontFloor frontCeiling) _ _ <- go mkFrontSector
             backSector <- traverse go mkBackSector
             -- We only draw the lower- or upper-front segments of a wall if the
             -- back sector is visible.
             let (drawLower,drawUpper) =
                   fromMaybe (False,False)
                             (do GLSector back _ _ <- backSector
                                 return (sectorFloor back > frontFloor
                                        ,sectorCeiling back < frontCeiling))
             -- Allocate a buffer for the vertex data.
             vbo <- overPtr (glGenBuffers 1)
             glBindBuffer GL_ARRAY_BUFFER vbo
             glBufferData
               GL_ARRAY_BUFFER
               (fromIntegral
                  (sizeOf (undefined :: GL.Vertex) *
                   (4 +
                    (if drawLower
                        then 4
                        else 0) +
                    (if drawUpper
                        then 4
                        else 0))))
               nullPtr
               GL_STATIC_DRAW
             -- Upload vertex data.
             let wallV = v2 ^-^ v1
                 wallLen = norm wallV
                 frontHeight = frontCeiling - frontFloor
                 u =
                   realToFrac (wallLen / textureSize) :: CFloat
                 v =
                   realToFrac (frontHeight / textureSize) -- TODO Should depend on bottom/top in mkVertices
                 mkVertices bottom top =
                   getZipList
                     (GL.Vertex <$>
                      (fmap realToFrac <$>
                       ZipList [V3 (v1 ^. _x)
                                   bottom
                                   (v1 ^. _y)
                               ,V3 (v2 ^. _x)
                                   bottom
                                   (v2 ^. _y)
                               ,V3 (v2 ^. _x)
                                   top
                                   (v2 ^. _y)
                               ,V3 (v1 ^. _x)
                                   top
                                   (v1 ^. _y)]) <*>
                      ZipList (repeat (normalize (case perp wallV of
                                                    V2 x y ->
                                                      realToFrac <$>
                                                      V3 x 0 y))) <*>
                      ZipList (repeat (normalize (case wallV of
                                                    V2 x y ->
                                                      realToFrac <$>
                                                      V3 x 0 y))) <*>
                      ZipList (repeat (V3 0 1 0)) <*>
                      ZipList [V2 0 v,V2 0 0,V2 u 0,V2 u v])
             -- Upload the main wall segment vertex data
             let sizeOfWall =
                   4 *
                   fromIntegral (sizeOf (undefined :: GL.Vertex))
             withArray (mkVertices frontFloor frontCeiling)
                       (glBufferSubData GL_ARRAY_BUFFER 0 sizeOfWall .
                        castPtr)
             -- Upload the upper-front wall segment, if necessary.
             for_ backSector $
               \(GLSector (SectorProperties _ backCeiling) _ _) ->
                 when drawUpper
                      (withArray (mkVertices backCeiling frontCeiling)
                                 (glBufferSubData GL_ARRAY_BUFFER sizeOfWall sizeOfWall .
                                  castPtr))
             -- Upload the lower-front wall segment, if necessary.
             for_ backSector $
               \(GLSector (SectorProperties backFloor _) _ _) ->
                 when drawLower
                      (withArray (mkVertices frontFloor backFloor)
                                 (glBufferSubData
                                    GL_ARRAY_BUFFER
                                    (if drawUpper
                                        then 2 * sizeOfWall
                                        else sizeOfWall)
                                    sizeOfWall .
                                  castPtr))
             GL.configureVertexAttributes
             return (GLWall (do glBindVertexArray vao
                                when (not (drawUpper || drawLower))
                                     (glDrawArrays GL_TRIANGLE_FAN 0 4) -- TODO
                                when drawUpper (glDrawArrays GL_TRIANGLE_FAN 4 4)
                                when drawLower
                                     (glDrawArrays
                                        GL_TRIANGLE_FAN
                                        (if drawUpper
                                            then 8
                                            else 4)
                                        4)))

testWorld :: PWorld TWorld
testWorld =
  PWorld (letrec (\_ ->
                    Hadoom.World.Material (Texture "stonework-diffuse.png")
                                          (Just (Texture "stonework-normals.png")) :::
                    Vertex (V2 (-10)
                               (-10)) :::
                    Vertex (V2 (-2)
                               (-10)) :::
                    Vertex (V2 2 (-10)) :::
                    Vertex (V2 10 (-10)) :::
                    Vertex (V2 10 10) :::
                    Vertex (V2 (-10) 10) :::
                    Vertex (V2 3 (-40)) :::
                    Vertex (V2 (-3)
                               (-40)) :::
                    TNil)
                 (\(m ::: v1 ::: v2 ::: v3 ::: v4 ::: v5 ::: v6 ::: v7 ::: v8 ::: _) ->
                    letrec (\_ ->
                              Sector (SectorProperties (-3)
                                                       10)
                                     [v1,v2,v3,v4,v5,v6]
                                     m
                                     m
                                     m :::
                              Sector (SectorProperties (-2)
                                                       5)
                                     [v2,v8,v7,v3]
                                     m
                                     m
                                     m :::
                              TNil)
                           (\(s1 ::: s2 ::: TNil) ->
                              World [s1,s2]
                                    [Wall v1 v2 s1 Nothing
                                    ,Wall v2 v3 s1 (Just s2)
                                    ,Wall v3 v4 s1 Nothing
                                    ,Wall v4 v5 s1 Nothing
                                    ,Wall v5 v6 s1 Nothing
                                    ,Wall v6 v1 s1 Nothing
                                    ,Wall v2 v8 s2 Nothing
                                    ,Wall v8 v7 s2 Nothing
                                    ,Wall v7 v3 s2 Nothing])))

textureSize :: Float
textureSize = 2.5
