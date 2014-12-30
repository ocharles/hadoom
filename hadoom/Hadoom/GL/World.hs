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

data CompiledWallFace =
  CompiledWallFace {cwfLower :: Maybe WallSegment
                   ,cwfUpper :: Maybe WallSegment
                   ,cwfMiddle :: Maybe WallSegment}

data CompiledWall =
  CompiledWall {cwFront :: CompiledWallFace
               ,cwBack :: Maybe CompiledWallFace}

data CompiledWorld =
  CompiledWorld {cwSectors :: [CompiledSector]
                ,cwWalls :: [CompiledWall]}

data GLInterpretation :: SceneElementType -> * where
  GLVertex :: V2 Float -> GLInterpretation TVertex
  GLWall :: CompiledWall -> GLInterpretation TWall
  GLWallFace :: CompiledSector -> Maybe CompiledMaterial -> Maybe CompiledMaterial -> Maybe CompiledMaterial -> GLInterpretation TWallFace
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
                  let drawWallFace CompiledWallFace{..} =
                        do for_ cwfLower
                                (\WallSegment{..} ->
                                   do Material.activateMaterial wsMat
                                      wsRender)
                           for_ cwfUpper
                                (\WallSegment{..} ->
                                   do Material.activateMaterial wsMat
                                      wsRender)
                           for_ cwfMiddle
                                (\WallSegment{..} ->
                                   do Material.activateMaterial wsMat
                                      wsRender)
                  in do drawWallFace cwFront
                        traverse drawWallFace cwBack)
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
                  let drawWallFace CompiledWallFace{..} =
                        do for_ cwfLower (\WallSegment{..} -> wsRender)
                           for_ cwfUpper (\WallSegment{..} -> wsRender)
                           for_ cwfMiddle (\WallSegment{..} -> wsRender)
                  in do drawWallFace cwFront
                        traverse drawWallFace cwBack)
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
        go (Wall mkV1 mkV2 mkFrontFace mkBackFace) =
          do
             -- Allocate a vertex array object for this wall.
             vao <- overPtr (glGenVertexArrays 1)
             glBindVertexArray vao
             -- Evaluate the start and end vertices, and the front and back sectors.
             GLVertex v1 <- go mkV1
             GLVertex v2 <- go mkV2
             GLWallFace frontSector frontLowerMat frontUpperMat frontMidMat <- go mkFrontFace
             mbackFace <- traverse go mkBackFace
             -- We only draw the lower- or upper-front segments of a wall if the
             -- back sector is visible.
             let (drawLower,drawUpper) =
                   fromMaybe (False,False)
                             (do GLWallFace backSector _ _ _ <- mbackFace
                                 return (sectorFloor (csProperties backSector) >
                                         sectorFloor (csProperties frontSector)
                                        ,sectorCeiling (csProperties backSector) <
                                         sectorCeiling (csProperties frontSector)))
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
                   sectorCeiling (csProperties frontSector) -
                   sectorFloor (csProperties frontSector)
                 u =
                   realToFrac (wallLen / textureSize) :: CFloat
                 -- v =
                 --   realToFrac (frontHeight / textureSize) -- TODO Should depend on bottom/top in mkVertices
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
                      ZipList [V2 u 1,V2 0 1,V2 0 0,V2 u 0])
             -- -- Upload the main wall segment vertex data
             let sizeOfWall =
                   4 *
                   fromIntegral (sizeOf (undefined :: GL.Vertex))
             withArray (mkVertices
                          (case mbackFace of
                             Just (GLWallFace s _ _ _) ->
                               max (sectorFloor (csProperties frontSector))
                                   (sectorFloor (csProperties s))
                             Nothing ->
                               sectorFloor (csProperties frontSector))
                          (case mbackFace of
                             Just (GLWallFace s _ _ _) ->
                               min (sectorCeiling (csProperties frontSector))
                                   (sectorCeiling (csProperties s))
                             Nothing ->
                               sectorCeiling (csProperties frontSector)))
                       (glBufferSubData GL_ARRAY_BUFFER 0 sizeOfWall .
                        castPtr)
             -- Upload the upper-front wall segment, if necessary.
             for_ mbackFace $
               \(GLWallFace backSector _ _ _) ->
                 when drawUpper
                      (withArray (mkVertices (sectorCeiling (csProperties backSector))
                                             (sectorCeiling (csProperties frontSector)))
                                 (glBufferSubData GL_ARRAY_BUFFER sizeOfWall sizeOfWall .
                                  castPtr))
             -- Upload the lower-front wall segment, if necessary.
             for_ mbackFace $
               \(GLWallFace back _ _ _) ->
                 when drawLower
                      (withArray (mkVertices (sectorFloor (csProperties frontSector))
                                             (sectorFloor (csProperties back)))
                                 (glBufferSubData
                                    GL_ARRAY_BUFFER
                                    (if drawUpper
                                        then 2 * sizeOfWall
                                        else sizeOfWall)
                                    sizeOfWall .
                                  castPtr))
             GL.configureVertexAttributes
             return
               (GLWall (CompiledWall
                          (CompiledWallFace
                             ((mbackFace >> frontLowerMat) <&>
                              (\mat ->
                                 WallSegment
                                   mat
                                   (do glBindVertexArray vao
                                       glDrawArrays
                                         GL_TRIANGLE_FAN
                                         (if drawUpper
                                             then 8
                                             else 4)
                                         4)))
                             ((mbackFace >> frontUpperMat) <&>
                              (\mat ->
                                 WallSegment
                                   mat
                                   (do glBindVertexArray vao
                                       glDrawArrays GL_TRIANGLE_FAN 4 4)))
                             (frontMidMat <&>
                              (\mat ->
                                 WallSegment
                                   mat
                                   (do glBindVertexArray vao
                                       glDrawArrays GL_TRIANGLE_FAN 0 4))))
                          Nothing))
        go (WallFace s mkLower mkUpper mkMid) =
          GLWallFace <$>
             (go s >>= \(GLSector s) -> return s) <*>
             traverse (go >=> \(GLMaterial m) -> return m) mkLower <*>
             traverse (go >=> \(GLMaterial m) -> return m) mkUpper <*>
               traverse (go >=> \(GLMaterial m) -> return m) mkMid

textureSize :: Float
textureSize = 2.5
