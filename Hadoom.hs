module Main where

import Control.Lens
import Data.Distributive (distribute)
import Data.Function (fix)
import Foreign (Ptr, alloca, castPtr, nullPtr, peek, sizeOf, with)
import Foreign.C (CFloat, withCString)
import Graphics.Rendering.OpenGL (($=))

import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Graphics.UI.SDL.Basic as SDL
import qualified Graphics.UI.SDL.Video as SDL
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Data.Vector.Storable as V

import Linear as L

import Paths_hadoom

triangleTranslation :: Floating a => M44 a
triangleTranslation = eye4 & translation .~ V3 0 0 (-5)

main :: IO ()
main =
  alloca $ \winPtr ->
  alloca $ \rendererPtr -> do
    _ <- SDL.init 0x00000020
    _ <- SDL.createWindowAndRenderer 800 600 0 winPtr rendererPtr

    win <- peek winPtr

    withCString "Hadoom" $ SDL.setWindowTitle win

    GL.clearColor $= GL.Color4 0.5 0.5 0.5 1

    let vertices :: V.Vector CFloat
        vertices = V.fromList [ 0, 1, 0, -1, -1, 0, 1, -1, 0 ]

    vbo <- GL.genObjectName
    GL.bindBuffer GL.ArrayBuffer $= Just vbo

    V.unsafeWith vertices $ \verticesPtr ->
      GL.bufferData GL.ArrayBuffer $= (fromIntegral (3 * 3 * sizeOf (0 :: CFloat)), verticesPtr, GL.StaticDraw)

    GL.vertexAttribPointer positionAttribute $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 nullPtr)
    GL.vertexAttribArray positionAttribute $= GL.Enabled

    shaderProg <- createShaderProgram "shaders/vertex/projection-model.glsl" "shaders/fragment/solid-white.glsl"
    GL.currentProgram $= Just shaderProg

    let perspective =
          let fov = 90
              s = recip (tan $ fov * 0.5 * pi / 180)
              far = 1000
              near = 1
          in V.fromList [ s, 0, 0, 0
                        , 0, s, 0, 0
                        , 0, 0, -(far/(far - near)), -1
                        , 0, 0, -((far*near)/(far-near)), 1
                        ]

    V.unsafeWith perspective $ \ptr -> do
      GL.UniformLocation loc <- GL.get (GL.uniformLocation shaderProg "projection")
      GL.glUniformMatrix4fv loc 1 0 ptr

    (fix $ \f n -> do
       GL.clearColor $= GL.Color4 0.5 0.2 1 1
       GL.clear [GL.ColorBuffer]
       GL.UniformLocation loc <- GL.get (GL.uniformLocation shaderProg "model")
       with (distribute (triangleTranslation !*! (eye4 & translation .~ V3 0 0 n))) $ \ptr ->
         GL.glUniformMatrix4fv loc 1 0 (castPtr (ptr :: Ptr (M44 CFloat)))
       GL.drawArrays GL.Triangles 0 3
       SDL.glSwapWindow win
       f (n + 0.001)) 0

positionAttribute :: GL.AttribLocation
positionAttribute = GL.AttribLocation 0

createShaderProgram :: FilePath -> FilePath -> IO GL.Program
createShaderProgram vertexShaderPath fragmentShaderPath = do
  vertexShader <- GL.createShader GL.VertexShader
  fragmentShader <- GL.createShader GL.FragmentShader

  compileShader vertexShaderPath vertexShader
  compileShader fragmentShaderPath fragmentShader

  shaderProg <- GL.createProgram
  GL.attachShader shaderProg vertexShader
  GL.attachShader shaderProg fragmentShader
  GL.attribLocation shaderProg "in_Position" $= positionAttribute
  GL.linkProgram shaderProg

  return shaderProg

  where
  compileShader path shader = do
    src <- getDataFileName path >>= Text.readFile
    GL.shaderSourceBS shader $= Text.encodeUtf8 src
    GL.compileShader shader
