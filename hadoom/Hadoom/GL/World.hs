{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

-- | This module allows a 'Hadoom.World.World' to be compiled into a description
-- that can be viewed via OpenGL.
module Hadoom.GL.World where

import BasePrelude
import Control.Lens hiding (indices)
import Control.Monad.Fix
import Data.TList
import Foreign
import Foreign.C.Types
import Graphics.GL
import Hadoom.Geometry
import Hadoom.World
import Linear
import Material
import Util
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Hadoom.GL.Vertex as GL

type CompiledMaterial = Material.Material

data CompiledSector =
  CompiledSector {csRenderFloor :: IO ()
                 ,csRenderCeiling :: IO ()
                 ,csFloorMat :: CompiledMaterial
                 ,csCeilingMat :: CompiledMaterial
                 ,csProperties :: SectorProperties}

data WallSegment =
  WallSegment {wsMat :: CompiledMaterial
              ,wsRender :: IO ()}

data CompiledWall =
  CompiledWall { cwLower :: Maybe WallSegment
               , cwUpper :: Maybe WallSegment
               , cwMiddle :: Maybe WallSegment
               }

data CompiledWorld =
  CompiledWorld {cwSectors :: [CompiledSector]
                ,cwWalls :: [CompiledWall]}

data GLInterpretation :: SceneElementType -> * where
  GLVertex :: V2 Float -> GLInterpretation TVertex
  GLWall :: CompiledWall -> GLInterpretation TWall
  GLSector :: CompiledSector -> GLInterpretation TSector
  GLWorld :: CompiledWorld -> GLInterpretation TWorld
  GLTexture :: GLTextureObject -> GLInterpretation TTexture
  GLMaterial :: Material.Material -> GLInterpretation TMaterial

-- | Given a compiled world, render the entire world with the correct materials.
drawWorldTextured :: CompiledWorld -> IO ()
drawWorldTextured (CompiledWorld sectors walls) =
  do traverse_ (\CompiledSector{..} ->
                  do Material.activateMaterial csFloorMat
                     csRenderFloor
                     Material.activateMaterial csCeilingMat
                     csRenderCeiling)
               sectors
     traverse_ (\CompiledWall{..} ->
                  do for_ cwLower
                          (\WallSegment{..} ->
                             do Material.activateMaterial wsMat
                                wsRender)
                     for_ cwUpper
                          (\WallSegment{..} ->
                             do Material.activateMaterial wsMat
                                wsRender)
                     for_ cwMiddle
                          (\WallSegment{..} ->
                             do Material.activateMaterial wsMat
                                wsRender))
               walls

-- | Given a compiled world, render everything, but do not change materials.
-- Used for prefilling the depth buffer, or rendering depth for light shadow
-- maps.
drawWorldGeometry :: CompiledWorld -> IO ()
drawWorldGeometry (CompiledWorld sectors walls) =
  do traverse_ (\CompiledSector{..} ->
                  do csRenderFloor
                     csRenderCeiling)
               sectors
     traverse_ (\CompiledWall{..} ->
                  do for_ cwLower (\WallSegment{..} -> wsRender)
                     for_ cwUpper (\WallSegment{..} -> wsRender)
                     for_ cwMiddle (\WallSegment{..} -> wsRender))
               walls

-- | Compile a 'PWorld' into objects that can be used by OpenGL.
compile :: PWorld l -> IO (GLInterpretation l)
compile (PWorld levelExpr) = go levelExpr
  where go :: WorldExpr GLInterpretation t -> IO (GLInterpretation t)
        go (Let bindings in_) =
          go . in_ =<<
          mfix (ttraverse go . bindings)
        go (Var x) = return x
        go (Vertex v) = return (GLVertex v)
        go (Texture path colorSpace) =
          GLTexture <$>
          loadTexture path colorSpace
        go (Hadoom.World.Material mkDiffuse mkNormals) =
          do GLMaterial <$>
               (Material.Material <$>
                ((\(GLTexture t) -> t) <$>
                 go mkDiffuse) <*>
                ((\(Just (GLTexture t)) -> t) <$>
                 traverse go mkNormals))
        go (World mkSectors mkWalls) =
          do sectors <- traverse (go >=> \(GLSector s) -> return s) mkSectors
             walls <- traverse (go >=> \(GLWall w) -> return w) mkWalls
             return (GLWorld (CompiledWorld sectors walls))
        go (Sector props mkVertices mkFloorMat mkCeilingMat) =
          do
             -- Realise all vertices
             vertices <- map (\(GLVertex v) -> v) <$>
                         traverse go mkVertices
             let floorVertices =
                   vertices <&>
                   \(V2 x y) ->
                     GL.Vertex (realToFrac <$>
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
                     GL.Vertex (p & _y .~
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
                   triangulate (V.fromList vertices)
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
             GLSector <$>
               (CompiledSector
                  (do glBindVertexArray vao
                      glDrawElements GL_TRIANGLES
                                     (fromIntegral (V.length floorIndices))
                                     GL_UNSIGNED_INT
                                     nullPtr)
                  (do glBindVertexArray vao
                      glDrawElements
                        GL_TRIANGLES
                        (fromIntegral (V.length ceilingIndices))
                        GL_UNSIGNED_INT
                        (nullPtr `plusPtr`
                         fromIntegral
                           (sizeOf (0 :: GLuint) *
                            V.length floorIndices))) <$>
                ((\(GLMaterial m) -> m) <$>
                 go mkFloorMat) <*>
                ((\(GLMaterial m) -> m) <$>
                 go mkCeilingMat) <*>
                pure props)
        go (Wall mkV1 mkV2 mkFrontSector mkBackSector mkMidMat mkUpperMat mkLowerMat) =
          do
             -- Allocate a vertex array object for this wall.
             vao <- overPtr (glGenVertexArrays 1)
             glBindVertexArray vao
             -- Evaluate the start and end vertices, and the front and back sectors.
             GLVertex v1 <- go mkV1
             GLVertex v2 <- go mkV2
             GLSector compiledSector <- go mkFrontSector
             backSector <- traverse go mkBackSector
             -- We only draw the lower- or upper-front segments of a wall if the
             -- back sector is visible.
             let (drawLower,drawUpper) =
                   fromMaybe (False,False)
                             (do GLSector back <- backSector
                                 return (sectorFloor (csProperties back) >
                                         sectorFloor (csProperties compiledSector)
                                        ,sectorCeiling (csProperties back) <
                                         sectorCeiling (csProperties compiledSector)))
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
                 frontHeight =
                   sectorCeiling (csProperties compiledSector) -
                   sectorFloor (csProperties compiledSector)
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
             withArray (mkVertices (case backSector of
                                      Just (GLSector s) -> max (sectorFloor (csProperties compiledSector)) (sectorFloor (csProperties s))
                                      Nothing -> sectorFloor (csProperties compiledSector))
                                   (case backSector of
                                      Just (GLSector s) -> min (sectorCeiling (csProperties compiledSector)) (sectorCeiling (csProperties s))
                                      Nothing -> sectorCeiling (csProperties compiledSector)))
                       (glBufferSubData GL_ARRAY_BUFFER 0 sizeOfWall .
                        castPtr)
             -- Upload the upper-front wall segment, if necessary.
             for_ backSector $
               \(GLSector back) ->
                 when drawUpper
                      (withArray (mkVertices (sectorCeiling (csProperties back))
                                             (sectorCeiling (csProperties compiledSector)))
                                 (glBufferSubData GL_ARRAY_BUFFER sizeOfWall sizeOfWall .
                                  castPtr))
             -- Upload the lower-front wall segment, if necessary.
             for_ backSector $
               \(GLSector back) ->
                 when drawLower
                      (withArray (mkVertices (sectorFloor (csProperties compiledSector))
                                             (sectorFloor (csProperties back)))
                                 (glBufferSubData
                                    GL_ARRAY_BUFFER
                                    (if drawUpper
                                        then 2 * sizeOfWall
                                        else sizeOfWall)
                                    sizeOfWall .
                                  castPtr))
             GL.configureVertexAttributes
             GLWall <$>
               (CompiledWall <$>
                (for (backSector >> mkLowerMat)
                     (\mkMat ->
                        WallSegment <$>
                        ((\(GLMaterial m) -> m) <$>
                         go mkMat) <*>
                        pure (do glBindVertexArray vao
                                 glDrawArrays
                                   GL_TRIANGLE_FAN
                                   (if drawUpper
                                       then 8
                                       else 4)
                                   4))) <*>
                (for (backSector >> mkUpperMat)
                     (\mkMat ->
                        WallSegment <$>
                        ((\(GLMaterial m) -> m) <$>
                         go mkMat) <*>
                        pure (do glBindVertexArray vao
                                 glDrawArrays GL_TRIANGLE_FAN 4 4))) <*>
                (for mkMidMat
                     (\mkMat ->
                        WallSegment <$>
                        ((\(GLMaterial m) -> m) <$>
                         go mkMat) <*>
                        pure (do glBindVertexArray vao
                                 glDrawArrays GL_TRIANGLE_FAN 0 4))))

textureSize :: Float
textureSize = 2.5
