{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Prelude hiding (any, floor, ceiling, (.), id)

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Lens hiding (indices)
import Control.Monad.Fix (MonadFix)
import Data.Distributive (distribute)
import Data.Foldable (any)
import Data.Int (Int32)
import Data.Monoid ((<>))
import Foreign.C (CFloat, withCString)
import Foreign (Ptr, Storable(..), alloca, castPtr, nullPtr, plusPtr, with)
import Graphics.Rendering.OpenGL (($=))
import Linear as L
import Data.Time (getCurrentTime, diffUTCTime)
import Unsafe.Coerce (unsafeCoerce)

import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JP
import qualified Data.IntMap.Strict as IM
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Graphics.UI.SDL.Basic as SDL
import qualified Graphics.UI.SDL.Enum as SDL
import qualified Graphics.UI.SDL.Event as SDL
import qualified Graphics.UI.SDL.Types as SDL
import qualified Graphics.UI.SDL.Video as SDL

import qualified FRP

import Paths_hadoom

data Sector =
  Sector {sectorVertices :: IM.IntMap (V2 CFloat)
         ,sectorWalls :: V.Vector (Int,Int)
         ,sectorFloor :: CFloat
         ,sectorCeiling :: CFloat
         ,sectorFloorTexture :: GL.TextureObject
         ,sectorCeilingTexture :: GL.TextureObject
         ,sectorWallTexture :: GL.TextureObject}

data Vertex =
  Vertex {vPos :: {-# UNPACK #-} !(V3 CFloat)
         ,vNorm :: {-# UNPACK #-} !(V3 CFloat)
         ,vUV :: {-# UNPACK #-} !(V2 CFloat)}
  deriving (Show)

instance Storable Vertex where
  sizeOf ~(Vertex p n uv) = sizeOf p + sizeOf n + sizeOf uv
  alignment ~(Vertex p _ _) = alignment p
  peek ptr = Vertex <$> peek (castPtr ptr)
                    <*> peek (castPtr $ ptr `plusPtr` sizeOf (vPos undefined))
                    <*> peek (castPtr $ ptr `plusPtr` sizeOf (vPos undefined) `plusPtr` sizeOf (vNorm undefined))
  poke ptr (Vertex p n uv) = do
    poke (castPtr $ ptr) p
    poke (castPtr $ ptr `plusPtr` sizeOf p) n
    poke (castPtr $ ptr `plusPtr` sizeOf p `plusPtr` sizeOf n) uv

triangleArea :: Fractional a => V2 a -> V2 a -> V2 a -> a
triangleArea a b c =
  let toV3 (V2 x y) = V3 x y 1
      det =
        det33 (V3 (toV3 a)
                  (toV3 b)
                  (toV3 c))
  in 0.5 * det

pointInTriangle :: (Fractional a, Ord a) => V2 a -> V2 a -> V2 a -> V2 a -> Bool
pointInTriangle p0@(V2 p0x p0y) p1@(V2 p1x p1y) p2@(V2 p2x p2y) (V2 px py) =
  let area = triangleArea p0 p1 p2
      s = 1 / (2 * area) * (p0y * p2x - p0x * p2y + (p2y - p0y) * px + (p0x - p2x) * py)
      t = 1 / (2 * area) * (p0x * p1y - p0y * p1x + (p0y - p1y) * px + (p1x - p0x) * py)
  in s > 0 && t > 0 && (1 - s - t) > 0

triangulate :: (Fractional a, Ord a) => V.Vector (V2 a) -> V.Vector Int
triangulate = go . addIndices
  where takeFirst f = V.take 1 . V.filter f

        isEar ((_,a),(_,b),(_,c),otherVertices) =
          let area = triangleArea a b c
              containsOther =
                any (pointInTriangle a b c . snd)
                    otherVertices
          in area > 0 && not containsOther

        go s
          | V.length s < 3 = empty
          | otherwise =
            do (v0@(n0,_),(n1,_),v2@(n2,_),others) <- takeFirst isEar (separate s)
               [n0,n2,n1] <> go (v0 `V.cons` (v2 `V.cons` others))

        addIndices vertices =
          V.zip [0 .. V.length vertices] vertices

        separate vertices =
          let n = V.length vertices
              doubleVerts = vertices <> vertices
          in V.zip4 vertices
                    (V.drop 1 doubleVerts)
                    (V.drop 2 doubleVerts)
                    (V.imap (\i _ ->
                               V.take (n - 3) $
                               V.drop (i + 3) $
                               doubleVerts)
                            vertices)

realiseSector :: Sector -> IO (IO ())
realiseSector Sector{..} =
  do vao <- GL.genObjectName :: IO (GL.VertexArrayObject)
     GL.bindVertexArrayObject $= Just vao

     vbo <- GL.genObjectName
     GL.bindBuffer GL.ArrayBuffer $=
       Just vbo

     let expandEdge start@(V2 x1 y1) end@(V2 x2 y2) =
           let n =
                 case normalize $ perp $ end ^-^ start of
                   V2 x y -> V3 x 0 y
           in V.fromList $ getZipList $ Vertex <$>
              ZipList [V3 x1 sectorFloor y1
                      ,V3 x1 sectorCeiling y1
                      ,V3 x2 sectorFloor y2
                      ,V3 x2 sectorCeiling y2] <*>
              ZipList (repeat n) <*>
              ZipList [V2 0 0,V2 0 1,V2 1 0,V2 1 1]

         wallVertices =
           V.concatMap
             (\(s,e) ->
                expandEdge (sectorVertices IM.! s)
                           (sectorVertices IM.! e))
             sectorWalls

         floorVertices =
           V.map (\(V2 x y) ->
                    Vertex (V3 x sectorFloor y)
                           (V3 0 1 0)
                           (V2 x y ^*
                            5.0e-2))
                 (V.fromList $ IM.elems sectorVertices)

         ceilingVertices =
           V.map (\(Vertex p n uv) ->
                    Vertex (p & _y .~ sectorCeiling)
                           (negate n)
                           uv)
                 floorVertices

         vertices = wallVertices <> floorVertices <> ceilingVertices

     SV.unsafeWith (V.convert vertices) $
       \verticesPtr ->
         GL.bufferData GL.ArrayBuffer $=
         (fromIntegral
            (V.length vertices *
             sizeOf (undefined :: Vertex))
         ,verticesPtr
         ,GL.StaticDraw)

     let stride = fromIntegral $ sizeOf (undefined :: Vertex)
         normalOffset = fromIntegral $ sizeOf (0 :: V3 CFloat)
         uvOffset = normalOffset + fromIntegral (sizeOf (0 :: V3 CFloat))

     GL.vertexAttribPointer positionAttribute $=
       (GL.ToFloat,GL.VertexArrayDescriptor 3 GL.Float stride nullPtr)
     GL.vertexAttribArray positionAttribute $= GL.Enabled

     GL.vertexAttribPointer normalAttribute $=
       (GL.ToFloat
       ,GL.VertexArrayDescriptor 3
                                 GL.Float
                                 stride
                                 (nullPtr `plusPtr` normalOffset))
     GL.vertexAttribArray normalAttribute $= GL.Enabled

     GL.vertexAttribPointer uvAttribute $=
       (GL.ToFloat
       ,GL.VertexArrayDescriptor 2
                                 GL.Float
                                 stride
                                 (nullPtr `plusPtr` uvOffset))
     GL.vertexAttribArray uvAttribute $= GL.Enabled

     let wallIndices =
           V.concatMap id $
           V.imap (\m _ ->
                     let n = m * 4
                     in V.map fromIntegral [n,n + 2,n + 1,n + 1,n + 2,n + 3])
                  sectorWalls

         floorIndices =
           let n = fromIntegral $ V.length wallVertices
           in fmap (fromIntegral . (+ n)) $
              triangulate (V.fromList $ IM.elems sectorVertices)

         ceilingIndices =
           V.map (+ (fromIntegral $ V.length floorVertices)) $
           V.concatMap id $
           V.zipWith3 (\a b c -> [a,c,b])
                      floorIndices
                      (V.drop 1 $ floorIndices <> floorIndices)
                      (V.drop 2 $ floorIndices <> floorIndices)

         indices :: V.Vector Int32
         indices = wallIndices <> floorIndices <> ceilingIndices

     ibo <- GL.genObjectName
     GL.bindBuffer GL.ElementArrayBuffer $= Just ibo

     SV.unsafeWith (V.convert indices) $
       \indicesPtr ->
         GL.bufferData GL.ElementArrayBuffer $=
         (fromIntegral
            (V.length indices *
             sizeOf (0 :: Int32))
         ,indicesPtr
         ,GL.StaticDraw)

     return $
       do GL.bindVertexArrayObject $= Just vao

          GL.textureBinding GL.Texture2D $= Just sectorWallTexture
          GL.drawElements GL.Triangles
                          (fromIntegral $ V.length wallIndices)
                          GL.UnsignedInt
                          nullPtr

          GL.textureBinding GL.Texture2D $= Just sectorFloorTexture
          GL.drawElements
            GL.Triangles
            (fromIntegral $ V.length floorIndices)
            GL.UnsignedInt
            (nullPtr `plusPtr`
             fromIntegral
               (sizeOf (0 :: Int32) *
                V.length wallIndices))

          GL.textureBinding GL.Texture2D $= Just sectorCeilingTexture
          GL.drawElements
            GL.Triangles
            (fromIntegral $ V.length ceilingIndices)
            GL.UnsignedInt
            (nullPtr `plusPtr`
             fromIntegral
               (sizeOf (0 :: Int32) *
                (V.length wallIndices + V.length floorIndices)))


data Light =
  Light {lightPos :: V3 CFloat
        ,lightColor :: V3 CFloat
        ,lightRadius :: CFloat}
  deriving (Show)


instance Storable Light where
  sizeOf _ =
    sizeOf (undefined :: V4 CFloat) *
    2
  alignment _ = sizeOf (undefined :: V4 CFloat)
  peek ptr =
    Light <$>
    peek (castPtr ptr) <*>
    peek (castPtr ptr `plusPtr`
          fromIntegral (sizeOf (undefined :: V4 CFloat))) <*>
    peek (castPtr ptr `plusPtr`
          fromIntegral
            (sizeOf (undefined :: V4 CFloat) +
             sizeOf (undefined :: V3 CFloat)))
  poke ptr (Light pos col r) =
    do poke (castPtr ptr) pos
       poke (castPtr $ ptr `plusPtr`
             fromIntegral (sizeOf (undefined :: V4 CFloat)))
            col
       poke (castPtr $ ptr `plusPtr`
             fromIntegral
               (sizeOf (undefined :: V4 CFloat) +
                sizeOf (undefined :: V3 CFloat)))
            r

main :: IO ()
main =
  alloca $ \winPtr ->
  alloca $ \rendererPtr -> do
    _ <- SDL.init SDL.initFlagEverything
    _ <- SDL.createWindowAndRenderer 1280 1024 0 winPtr rendererPtr
    win <- peek winPtr
    withCString "Hadoom" $ SDL.setWindowTitle win
    GL.clearColor $= GL.Color4 0.5 0.5 0.5 1
    GL.cullFace $= Just GL.Back

    wall1 <- loadTexture "wall.jpg"
    wall2 <- loadTexture "wall-2.jpg"
    ceiling <- loadTexture "ceiling.jpg"
    floor <- loadTexture "floor.jpg"

    drawSector1 <-
      let vertices = IM.fromList $ zip [0 ..] [V2 (-50) (-50)
                                              ,V2 (-30) (-50)
                                              ,V2 (-30) (-30)
                                              ,V2 10 (-30)
                                              ,V2 10 (-50)
                                              ,V2 50 (-50)
                                              ,V2 50 50
                                              ,V2 30 50
                                              ,V2 30 60
                                              ,V2 10 60
                                              ,V2 10 61
                                              ,V2 (-10) 61
                                              ,V2 (-10) 60
                                              ,V2 (-40) 60
                                              ,V2 (-40) 50
                                              ,V2 (-50) 50]
      in realiseSector Sector {sectorVertices = vertices
                              ,sectorCeiling = 30
                              ,sectorFloor = 0
                              ,sectorWalls = [(0,1),(1,2),(2,3),(3,4),(4,5)
                                             ,(5,6),(6,7),(7,8),(8,9),(9,10)
                                             ,(11,12),(12,13),(13,14),(14,0)]
                              ,sectorFloorTexture = floor
                              ,sectorCeilingTexture = ceiling
                              ,sectorWallTexture = wall1}
    drawSector2 <-
      let vertices = IM.fromList $ zip [0 ..] [V2 (-30) 61
                                              ,V2 (-10) 61
                                              ,V2 10 61
                                              ,V2 30 61
                                              ,V2 30 100
                                              ,V2 (-30) 100]
      in realiseSector Sector {sectorVertices = vertices
                              ,sectorCeiling = 30
                              ,sectorFloor = 0
                              ,sectorWalls = [(0,1),(2,3),(3,4),(4,5),(5,0)]
                              ,sectorFloorTexture = floor
                              ,sectorCeilingTexture = ceiling
                              ,sectorWallTexture = wall2}

    shaderProg <- createShaderProgram "shaders/vertex/projection-model.glsl"
                                      "shaders/fragment/solid-white.glsl"
    GL.currentProgram $= Just shaderProg

    let perspective =
          let fov = 75
              s = recip (tan $ fov * 0.5 * pi / 180)
              far = 1000
              near = 1
          in [s ,0 ,0 ,0
             ,0 ,s ,0 ,0
             ,0 ,0 ,-(far / (far - near)) ,-1
             ,0 ,0 ,-((far * near) / (far - near)) ,1]

    SV.unsafeWith perspective $
      \ptr ->
        do GL.UniformLocation loc <- GL.get (GL.uniformLocation shaderProg "projection")
           GL.glUniformMatrix4fv loc 1 0 ptr

    do GL.UniformLocation loc <- GL.get (GL.uniformLocation shaderProg "tex")
       GL.glUniform1i loc 0

    GL.depthFunc $= Just GL.Less

    lightsUBO <- GL.genObjectName

    shaderId <- unsafeCoerce shaderProg
    lightsUBI <- withCString "light" $ GL.glGetUniformBlockIndex shaderId

    GL.glUniformBlockBinding shaderId lightsUBI 0

    GL.bindBufferRange GL.IndexedUniformBuffer 0 $= Just (lightsUBO, 0, fromIntegral (sizeOf (undefined :: Light) * 1))

    GL.bindBuffer GL.UniformBuffer $= Just lightsUBO

    t0 <- getCurrentTime
    gameLoop win shaderProg (do GL.textureBinding GL.Texture2D $= Just wall1
                                drawSector1
                                GL.textureBinding GL.Texture2D $= Just wall2
                                drawSector2) scene
             t0

loadTexture :: FilePath -> IO GL.TextureObject
loadTexture path =
  do x <- JP.readImage path
     case x of
       Right (JP.ImageYCbCr8 img) ->
         do GL.activeTexture $=
              GL.TextureUnit 0
            t <- GL.genObjectName
            GL.textureBinding GL.Texture2D $=
              Just t
            GL.textureFilter GL.Texture2D $=
              ((GL.Linear',Just GL.Linear'),GL.Linear')
            let toRgb8 =
                  JP.convertPixel :: JP.PixelYCbCr8 -> JP.PixelRGB8
                toRgbF =
                  JP.promotePixel :: JP.PixelRGB8 -> JP.PixelRGBF
            case JP.pixelMap (toRgbF . toRgb8)
                             img of
              JP.Image w h d ->
                do SV.unsafeWith d $
                     \ptr ->
                       GL.texImage2D
                         GL.Texture2D
                         GL.NoProxy
                         0
                         GL.RGB32F
                         (GL.TextureSize2D (fromIntegral w)
                                           (fromIntegral h))
                         0
                         (GL.PixelData GL.RGB GL.Float ptr)
                   GL.generateMipmap' GL.Texture2D
                   GL.get GL.maxTextureMaxAnisotropy >>= print
                   GL.textureMaxAnisotropy GL.Texture2D $= 16
                   return t
       Left e -> error e
       _ -> error "Unknown image format"

gameLoop win shaderProg drawSector w currentTime = do
  newTime <- getCurrentTime
  let frameTime = newTime `diffUTCTime` currentTime

  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  events <- unfoldEvents
  let FRP.Out (Scene viewMat lights) w' = runIdentity $
        FRP.stepWire (realToFrac frameTime) events w

  with (distribute viewMat) $ \ptr -> do
    GL.UniformLocation loc <- GL.get (GL.uniformLocation shaderProg "view")
    GL.glUniformMatrix4fv loc 1 0 (castPtr (ptr :: Ptr (M44 CFloat)))

  let lights' = flip SV.map lights $ \(Light (V3 x y z) col r) ->
        Light ((viewMat !* V4 x y z 1) ^. _xyz) col r
  SV.unsafeWith lights' $ \ptr -> do
    GL.bufferData GL.UniformBuffer $= (fromIntegral (sizeOf (undefined :: Light) * SV.length lights'), ptr, GL.StreamDraw)

  _ <- drawSector

  SDL.glSwapWindow win

  gameLoop win shaderProg drawSector w' newTime

unfoldEvents :: IO [SDL.Event]
unfoldEvents = alloca $ \evtPtr -> do
  r <- SDL.pollEvent evtPtr
  case r of
    0 -> return []
    _ -> (:) <$> peek evtPtr <*> unfoldEvents

positionAttribute :: GL.AttribLocation
positionAttribute = GL.AttribLocation 0

normalAttribute :: GL.AttribLocation
normalAttribute = GL.AttribLocation 1

uvAttribute :: GL.AttribLocation
uvAttribute = GL.AttribLocation 2

createShaderProgram :: FilePath -> FilePath -> IO GL.Program
createShaderProgram vertexShaderPath fragmentShaderPath = do
  vertexShader <- GL.createShader GL.VertexShader
  compileShader vertexShaderPath vertexShader

  fragmentShader <- GL.createShader GL.FragmentShader
  compileShader fragmentShaderPath fragmentShader

  shaderProg <- GL.createProgram
  GL.attachShader shaderProg vertexShader
  GL.attachShader shaderProg fragmentShader

  GL.attribLocation shaderProg "in_Position" $= positionAttribute
  GL.attribLocation shaderProg "in_Normal" $= normalAttribute
  GL.attribLocation shaderProg "in_UV" $= uvAttribute

  GL.linkProgram shaderProg

  return shaderProg

  where
  compileShader path shader = do
    src <- getDataFileName path >>= Text.readFile
    GL.shaderSourceBS shader $= Text.encodeUtf8 src
    GL.compileShader shader
    GL.get (GL.shaderInfoLog shader) >>= putStrLn

data Scene =
  Scene {sceneCamera :: M44 CFloat
        ,sceneLights :: SV.Vector Light}

scene :: FRP.Wire Identity [SDL.Event] Scene
scene = Scene <$> camera
              <*> (FRP.time <&> \t -> [ Light (V3 0 15 0) (V3 ((1 + realToFrac (sin t)) / 2) 1 0) (100 * realToFrac (1 + sin (t * 2)))
                                     , Light (V3 0 15 70) 1 1000])

camera :: FRP.Wire Identity [SDL.Event] (M44 CFloat)
camera = proc events -> do
  goForward <- keyHeld SDL.scancodeUp -< events
  goBack <- keyHeld SDL.scancodeDown -< events

  turnLeft <- keyHeld SDL.scancodeLeft -< events
  turnRight <- keyHeld SDL.scancodeRight -< events
  theta <- (FRP.integralWhen -< (-2, turnLeft)) + (FRP.integralWhen -< (2, turnRight))
  let quat = axisAngle (V3 0 1 0) theta

  rec position <- if goForward
                   then FRP.integral -< over _x negate $ rotate quat (V3 0 0 1) * 10
                   else returnA -< position'
      position' <- FRP.delay 0 -< position

  returnA -< m33_to_m44 (fromQuaternion quat) !*! mkTransformation 0 (position - V3 0 10 0)

keyPressed :: (Applicative m, MonadFix m) => SDL.Scancode -> FRP.Wire m [SDL.Event] Bool
keyPressed scancode = proc events -> do
  rec pressed <- FRP.delay False -<
                   pressed ||
                     (filter ((== SDL.eventTypeKeyDown) . SDL.eventType) events
                        `hasScancode` scancode)
  returnA -< pressed

keyReleased :: (Applicative m, MonadFix m) => SDL.Scancode -> FRP.Wire m [SDL.Event] Bool
keyReleased scancode =
  proc events ->
  do rec released <- FRP.delay False -<
                       released ||
                         (filter ((== SDL.eventTypeKeyUp) . SDL.eventType) events
                            `hasScancode` scancode)
     returnA -< released

keyHeld :: (Applicative m, MonadFix m) => SDL.Scancode -> FRP.Wire m [SDL.Event] Bool
keyHeld scancode =
  proc events ->
  do pressed <- keyPressed scancode -< events
     if pressed then
       do released <- keyReleased scancode -< events
          if released then FRP.delay False . keyHeld scancode -< events else
            returnA -< True
       else returnA -< False

hasScancode :: [SDL.Event] -> SDL.Scancode -> Bool
events `hasScancode` s =
  case events of
    (SDL.KeyboardEvent _ _ _ _ _ (SDL.Keysym scancode _ _)) : xs -> scancode == s || xs `hasScancode` s
    _ : xs -> xs `hasScancode` s
    [] -> False
