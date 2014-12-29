{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Level where

import Control.Applicative
import Control.Lens hiding (Level, indices)
import Control.Monad (when)
import Control.Monad.Fix
import Data.Foldable (for_, traverse_)
import Data.Maybe
import Data.Monoid
import Foreign (nullPtr, sizeOf, castPtr, withArray, plusPtr)
import Foreign.C.Types
import Graphics.GL
import Linear
import Material
import Shader
import Util
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Sector

data SceneElementId = VertexId | WallId | SectorId | MaterialId | TextureId | WorldId

infixr 8 :::

data TList :: (k -> *) -> [k] -> * where
  TNil :: TList f '[]
  (:::) :: f t -> TList f ts -> TList f (t ': ts)

tmap :: (forall a. f a -> g a) -> TList f as -> TList g as
tmap _ TNil = TNil
tmap f (x ::: xs) = f x ::: tmap f xs

ttraverse :: Applicative i => (forall a. f a -> i (g a)) -> TList f xs -> i (TList g xs)
ttraverse _ TNil = pure TNil
ttraverse f (x ::: xs) = (:::) <$> f x <*> ttraverse f xs

data LevelExpr :: (SceneElementId -> *) -> SceneElementId -> * where
  -- | The basic ability to introduce names for things
  Let :: (TList f ts -> TList (LevelExpr f) ts) -> (TList f ts -> LevelExpr f t) -> LevelExpr f t

  -- | The ability to use a name
  Var :: f t -> LevelExpr f t

  -- | 'Literal' vertices
  Vertex :: V2 Float -> LevelExpr f VertexId

  -- | Walls between vertices, with their two sectors
  Wall
    :: LevelExpr f VertexId -> LevelExpr f VertexId  -- Start and end vertices
    -> LevelExpr f SectorId -> Maybe (LevelExpr f SectorId) -- Front and (optional) back sector
    -> LevelExpr f WallId

  -- | Sectors take some basic properties, a list of vertices, and their material attributes
  Sector
    :: SectorProperties
    -> [LevelExpr f VertexId] -- vertices
    -> LevelExpr f MaterialId -- floor
    -> LevelExpr f MaterialId -- ceiling
    -> LevelExpr f MaterialId -- walls
    -> LevelExpr f SectorId

  -- | A material is the composition of a basic diffuse texture along with an optional normal map
  Material
    :: LevelExpr f TextureId
    -> Maybe (LevelExpr f TextureId)
    -> LevelExpr f MaterialId

  -- | A texture refers to an image on disk
  Texture :: FilePath -> LevelExpr f TextureId

  World
    :: [LevelExpr f SectorId]
    -> [LevelExpr f WallId]
    -> LevelExpr f WorldId

newtype Level t = Level (forall f. LevelExpr f t)

data SectorProperties =
  SectorProperties {sectorFloor :: Float
                   ,sectorCeiling :: Float}

data GLInterpretation :: SceneElementId -> * where
  GLVertex :: V2 Float -> GLInterpretation VertexId
  GLWall :: IO () -> GLInterpretation WallId
  GLSector :: SectorProperties -> IO () -> Material.Material -> GLInterpretation SectorId
  GLWorld :: [GLInterpretation SectorId] -> [GLInterpretation WallId] -> GLInterpretation WorldId
  GLTexture :: GLTextureObject -> GLInterpretation TextureId
  GLMaterial :: Material.Material -> GLInterpretation MaterialId

drawWorldTextured :: GLInterpretation WorldId -> IO ()
drawWorldTextured (GLWorld sectors walls) =
  do traverse_ (\(GLSector _ io mat) ->
                  do Material.activateMaterial mat
                     io)
               sectors
     traverse_ (\(GLWall io) -> io) walls

drawWorldGeometry :: GLInterpretation WorldId -> IO ()
drawWorldGeometry (GLWorld sectors walls) =
  do traverse_ (\(GLSector _ io _) -> io) sectors
     traverse_ (\(GLWall io) -> io) walls

compile :: Level l -> IO (GLInterpretation l)
compile (Level levelExpr) = do go levelExpr
  where go :: LevelExpr GLInterpretation t -> IO (GLInterpretation t)
        go (Let bindings in_) =
          do bindings' <- mfix (ttraverse go . bindings)
             go (in_ bindings')
        go (Var x) = return x
        go (Vertex v) = return (GLVertex v)
        go (Texture path) =
          GLTexture <$> loadTexture path SRGB
        go (Level.Material mkDiffuse mkNormals) = do
          GLMaterial <$> (Material.Material <$> ((\(GLTexture t) -> t) <$> go mkDiffuse) <*> ((\(Just (GLTexture t)) -> t) <$> traverse go mkNormals))
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
                     Sector.Vertex
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
                   \(Sector.Vertex p n t bn uv) ->
                     Sector.Vertex
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
                  (sizeOf (undefined :: Sector.Vertex) *
                   length allVertices))
               nullPtr
               GL_STATIC_DRAW
             withArray allVertices
                       (glBufferSubData
                          GL_ARRAY_BUFFER
                          0
                          (fromIntegral
                             (sizeOf (undefined :: Sector.Vertex) *
                              length allVertices)) .
                        castPtr)
             configureVertexAttributes
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
                  (sizeOf (undefined :: Sector.Vertex) *
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
                     (Sector.Vertex <$>
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
                   fromIntegral (sizeOf (undefined :: Sector.Vertex))
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
                                 (glBufferSubData GL_ARRAY_BUFFER (if drawUpper then 2 * sizeOfWall else sizeOfWall) sizeOfWall .
                                  castPtr))
             configureVertexAttributes
             return (GLWall (do glBindVertexArray vao
                                when (not (drawUpper || drawLower)) (glDrawArrays GL_TRIANGLE_FAN 0 4) -- TODO
                                when drawUpper (glDrawArrays GL_TRIANGLE_FAN 4 4)
                                when drawLower
                                     (glDrawArrays
                                        GL_TRIANGLE_FAN
                                        (if drawUpper
                                            then 8
                                            else 4)
                                        4)))


configureVertexAttributes :: IO ()
configureVertexAttributes =
  do let stride =
           fromIntegral (sizeOf (undefined :: Sector.Vertex))
         normalOffset =
           fromIntegral (sizeOf (0 :: V3 CFloat))
         tangentOffset =
           normalOffset +
           fromIntegral (sizeOf (0 :: V3 CFloat))
         bitangentOffset =
           tangentOffset +
           fromIntegral (sizeOf (0 :: V3 CFloat))
         uvOffset =
           bitangentOffset +
           fromIntegral (sizeOf (0 :: V3 CFloat))
     glVertexAttribPointer positionAttribute 3 GL_FLOAT GL_FALSE stride nullPtr
     glEnableVertexAttribArray positionAttribute
     glVertexAttribPointer normalAttribute
                           3
                           GL_FLOAT
                           GL_FALSE
                           stride
                           (nullPtr `plusPtr` normalOffset)
     glEnableVertexAttribArray normalAttribute
     glVertexAttribPointer tangentAttribute
                           3
                           GL_FLOAT
                           GL_FALSE
                           stride
                           (nullPtr `plusPtr` tangentOffset)
     glEnableVertexAttribArray tangentAttribute
     glVertexAttribPointer bitangentAttribute
                           3
                           GL_FLOAT
                           GL_FALSE
                           stride
                           (nullPtr `plusPtr` bitangentOffset)
     glEnableVertexAttribArray bitangentAttribute
     glVertexAttribPointer uvAttribute
                           2
                           GL_FLOAT
                           GL_FALSE
                           stride
                           (nullPtr `plusPtr` uvOffset)
     glEnableVertexAttribArray uvAttribute

letrec :: (TList (LevelExpr f) ts -> TList (LevelExpr f) ts)
       -> (TList (LevelExpr f) ts -> LevelExpr f t)
       -> LevelExpr f t
letrec es e =
  Let (\xs -> es (tmap Var xs))
      (\xs -> e (tmap Var xs))

testWorld :: Level WorldId
testWorld =
  Level (letrec (\_ ->
                   Level.Material (Texture "stonework-diffuse.png") (Just (Texture "stonework-normals.png")) :::
                   Vertex (V2 (-10) (-10)) :::
                   Vertex (V2 (-2) (-10)) :::
                   Vertex (V2 2 (-10)) :::
                   Vertex (V2 10 (-10)) :::
                   Vertex (V2 10 10) :::
                   Vertex (V2 (-10) 10) :::
                   Vertex (V2 3 (-40)) :::
                   Vertex (V2 (-3) (-40)) :::
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
