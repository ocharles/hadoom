{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
module Main where

import Prelude hiding (floor, ceiling, (.), id)

import Control.Arrow
import Control.Category
import Control.Applicative
import Control.Lens hiding (indices)
import Control.Monad.Fix (MonadFix)
import Data.Distributive (distribute)
import Data.Int (Int32)
import Data.Monoid ((<>))
import Foreign.C (CFloat, withCString)
import Foreign (Ptr, Storable(..), alloca, castPtr, nullPtr, plusPtr, with)
import Graphics.Rendering.OpenGL (($=))
import Linear as L

import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JP
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Vector.Storable as V
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Graphics.UI.SDL.Basic as SDL
import qualified Graphics.UI.SDL.Enum as SDL
import qualified Graphics.UI.SDL.Event as SDL
import qualified Graphics.UI.SDL.Types as SDL
import qualified Graphics.UI.SDL.Video as SDL

import qualified FRP

import Paths_hadoom

type Sector = V.Vector (V2 CFloat)

data Vertex = Vertex { vPos :: V3 CFloat
                     , vNorm :: V3 CFloat
                     , vUV :: V2 CFloat
                     }

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


realiseSector :: Sector -> IO (IO ())
realiseSector sectorVertices = do
  vbo <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just vbo

  let expandEdge start@(V2 x1 y1) end@(V2 x2 y2) =
        let n = case normalize $ perp $ end ^-^ start of
                  V2 x y -> V3 x 0 y
        in getZipList $ Vertex <$> ZipList [ V3 x1 (-20) y1
                                           , V3 x1   20  y1
                                           , V3 x2 (-20) y2
                                           , V3 x2   20  y2
                                           ]
                               <*> ZipList (repeat n)
                               <*> ZipList [ V2 0 0
                                           , V2 0 1
                                           , V2 1 0
                                           , V2 1 1
                                           ]

  let vertices = V.fromList $ concat $ zipWith expandEdge (V.toList sectorVertices)
                                                          (V.toList $ V.tail sectorVertices <> sectorVertices)

  V.unsafeWith vertices $ \verticesPtr ->
    GL.bufferData GL.ArrayBuffer $=
      (fromIntegral (V.length sectorVertices * 2 * 2 * (3 + 3 + 2) * sizeOf (0 :: CFloat)), verticesPtr, GL.StaticDraw)

  let indices :: V.Vector Int32
      indices = V.fromList $ concatMap (\n -> [ n, n + 1, n + 2, n + 1, n + 3, n + 2 ]) $
                               map fromIntegral $
                                 map (* 4) $
                                   [0 .. V.length sectorVertices]

  ibo <- GL.genObjectName
  GL.bindBuffer GL.ElementArrayBuffer $= Just ibo

  V.unsafeWith indices $ \indicesPtr ->
    GL.bufferData GL.ElementArrayBuffer $=
      (fromIntegral (V.length indices * sizeOf (0 :: Int32)), indicesPtr, GL.StaticDraw)

  return $
    GL.drawElements GL.Triangles (fromIntegral $ V.length indices) GL.UnsignedInt nullPtr

triangleTranslation :: Floating a => M44 a
triangleTranslation = eye4 & translation .~ V3 0 0 (-5)

main :: IO ()
main =
  alloca $ \winPtr ->
  alloca $ \rendererPtr -> do
    _ <- SDL.init SDL.initFlagEverything
    _ <- SDL.createWindowAndRenderer 800 600 0 winPtr rendererPtr

    win <- peek winPtr

    withCString "Hadoom" $ SDL.setWindowTitle win

    GL.clearColor $= GL.Color4 0.5 0.5 0.5 1

    drawSector <- realiseSector [ V2 (-50) (-50)
                               , V2 0 (-40)
                               , V2 50 (-50)
                               , V2 50 50
                               , V2 (-50) 50 ]

    let stride = fromIntegral $ sizeOf (undefined :: Vertex)
        normalOffset = fromIntegral $ sizeOf (0 :: V3 CFloat)
        uvOffset = normalOffset + fromIntegral (sizeOf (0 :: V3 CFloat))

    GL.vertexAttribPointer positionAttribute $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float stride nullPtr)
    GL.vertexAttribArray positionAttribute $= GL.Enabled

    GL.vertexAttribPointer normalAttribute $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float stride (nullPtr `plusPtr` normalOffset))
    GL.vertexAttribArray normalAttribute $= GL.Enabled

    GL.vertexAttribPointer uvAttribute $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float stride (nullPtr `plusPtr` uvOffset))
    GL.vertexAttribArray uvAttribute $= GL.Enabled

    shaderProg <- createShaderProgram "shaders/vertex/projection-model.glsl"
                                     "shaders/fragment/solid-white.glsl"
    GL.currentProgram $= Just shaderProg

    let perspective =
          let fov = 75
              s = recip (tan $ fov * 0.5 * pi / 180)
              far = 1000
              near = 1
          in [ s, 0, 0, 0
             , 0, s, 0, 0
             , 0, 0, -(far/(far - near)), -1
             , 0, 0, -((far*near)/(far-near)), 1
             ]

    V.unsafeWith perspective $ \ptr -> do
      GL.UniformLocation loc <- GL.get (GL.uniformLocation shaderProg "projection")
      GL.glUniformMatrix4fv loc 1 0 ptr

    x <- JP.readImage "wall-2.jpg"
    case x of
      Right (JP.ImageYCbCr8 img) -> do
        GL.activeTexture $= GL.TextureUnit 0
        t <- GL.genObjectName
        GL.textureBinding GL.Texture2D $= Just t
        GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
        let toRgb8 = JP.convertPixel :: JP.PixelYCbCr8 -> JP.PixelRGB8
            toRgbF = JP.promotePixel :: JP.PixelRGB8 -> JP.PixelRGBF
        case JP.pixelMap (toRgbF . toRgb8) img of
          JP.Image w h d -> V.unsafeWith d $ \ptr -> do
            GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGB32F
                          (GL.TextureSize2D (fromIntegral w) (fromIntegral h))
                          0 (GL.PixelData GL.RGB GL.Float ptr)

      Left e -> error e
      _ -> error "Unknown image format"

    do
      GL.UniformLocation loc <- GL.get (GL.uniformLocation shaderProg "tex")
      GL.glUniform1i loc 0

    gameLoop win shaderProg drawSector camera

gameLoop :: SDL.Window -> GL.Program -> IO a -> FRP.Wire Identity [SDL.Event] (M44 CFloat) -> IO b
gameLoop win shaderProg drawSector w = do
  GL.clear [GL.ColorBuffer]

  events <- unfoldEvents
  let FRP.Out viewMat w' = runIdentity $ FRP.stepWire 0.005 events w

  with (distribute viewMat) $ \ptr -> do
    GL.UniformLocation loc <- GL.get (GL.uniformLocation shaderProg "view")
    GL.glUniformMatrix4fv loc 1 0 (castPtr (ptr :: Ptr (M44 CFloat)))

  let lightPos = (viewMat !* (V4 0.5 0.5 0 1)) ^. _xyz
  with lightPos $ \ptr -> do
    GL.UniformLocation loc <- GL.get (GL.uniformLocation shaderProg "lightPos")
    GL.glUniform3fv loc 1 (castPtr ptr)

  _ <- drawSector

  SDL.glSwapWindow win

  gameLoop win shaderProg drawSector w'

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

camera' :: FRP.Wire Identity [SDL.Event] (M44 CFloat)
camera' = proc events -> do
  rec pressed <- FRP.delay False -< pressed || (filter ((== SDL.eventTypeKeyDown) . SDL.eventType) events `hasScancode` SDL.scancodeUp)
  returnA -< eye4
  -- turnRight <- keyHeld SDL.scancodeRight -< events

  -- quat <- axisAngle (V3 0 1 0) <$> FRP.integralWhen -< (1, turnRight)
  -- rec position <- if goForward
  --                  then FRP.integral -< over _x negate $ rotate quat (V3 0 0 1) * 1
  --                  else returnA -< position'
  --     position' <- FRP.delay 0 -< position

  -- returnA -< m33_to_m44 (fromQuaternion quat) !*! mkTransformation 0 position

camera :: FRP.Wire Identity [SDL.Event] (M44 CFloat)
camera = proc events -> do
  goForward <- keyHeld SDL.scancodeUp -< events
  turnRight <- keyHeld SDL.scancodeRight -< events

  quat <- axisAngle (V3 0 1 0) <$> FRP.integralWhen -< (1, turnRight)
  rec position <- if goForward
                   then FRP.integral -< over _x negate $ rotate quat (V3 0 0 1) * 1
                   else returnA -< position'
      position' <- FRP.delay 0 -< position

  returnA -< m33_to_m44 (fromQuaternion quat) !*! mkTransformation 0 position

keyPressed :: (Applicative m, MonadFix m) => SDL.Scancode -> FRP.Wire m [SDL.Event] Bool
keyPressed scancode = proc events -> do
  rec pressed <- FRP.delay False -< pressed || (filter ((== SDL.eventTypeKeyDown) . SDL.eventType) events `hasScancode` scancode)
  returnA -< pressed

keyReleased :: (Applicative m, MonadFix m) => SDL.Scancode -> FRP.Wire m [SDL.Event] Bool
keyReleased scancode = proc events -> do
  rec released <- FRP.delay False -< released || (filter ((== SDL.eventTypeKeyUp) . SDL.eventType) events `hasScancode` scancode)
  returnA -< released

keyHeld :: (Applicative m, MonadFix m) => SDL.Scancode -> FRP.Wire m [SDL.Event] Bool
keyHeld scancode = proc events -> do
  pressed <- keyPressed scancode -< events
  if pressed
    then do
      released <- keyReleased scancode -< events
      if released
         then FRP.delay False . keyHeld scancode -< events
         else returnA -< True

    else returnA -< False

hasScancode :: [SDL.Event] -> SDL.Scancode -> Bool
events `hasScancode` s =
  case events of
    [] -> False
    (SDL.KeyboardEvent _ _ _ _ _ (SDL.Keysym scancode _ _)) : xs -> scancode == s || xs `hasScancode` s
    _ : xs -> xs `hasScancode` s
