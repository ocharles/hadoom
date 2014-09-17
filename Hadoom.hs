module Main where

import Control.Applicative
import Control.Lens
import Control.Monad (forever)
import Data.Distributive (distribute)
import Foreign (Ptr, alloca, castPtr, nullPtr, peek, sizeOf, with)
import Foreign.C (CFloat, withCString)
import Foreign.Marshal (alloca)
import Foreign.Storable (peek, sizeOf)
import Graphics.Rendering.OpenGL (($=))

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Graphics.UI.SDL.Basic as SDL
import qualified Graphics.UI.SDL.Video as SDL
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Data.Vector.Storable as V

import Linear as L
import Linear ((!*!))

triangleTranslation :: Floating a => M44 a
triangleTranslation = eye4 & translation .~ V3 0 0 (-5)

main :: IO ()
main =
  alloca $ \winPtr ->
  alloca $ \rendererPtr -> do
    SDL.init 0x00000020
    SDL.createWindowAndRenderer 800 600 0 winPtr rendererPtr

    win <- peek winPtr
    renderer <- peek rendererPtr

    withCString "Hadoom" $ SDL.setWindowTitle win

    GL.clearColor $= GL.Color4 0.5 0.5 0.5 1

    let vertices :: V.Vector CFloat
        vertices = V.fromList [ 0, 1, 0, -1, -1, 0, 1, -1, 0 ]

    vbo <- GL.genObjectName
    GL.bindBuffer GL.ArrayBuffer $= Just vbo

    V.unsafeWith vertices $ \verticesPtr ->
      GL.bufferData GL.ArrayBuffer $= (fromIntegral (3 * 3 * sizeOf (0 :: CFloat)), verticesPtr, GL.StaticDraw)

    let shaderAttribute = GL.AttribLocation 0
    GL.vertexAttribPointer shaderAttribute $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 nullPtr)
    GL.vertexAttribArray shaderAttribute $= GL.Enabled

    vertexShader <- GL.createShader GL.VertexShader
    fragmentShader <- GL.createShader GL.FragmentShader
    GL.shaderSourceBS vertexShader $= Text.encodeUtf8
      (Text.pack $ unlines
      [ "#version 130"
      , "uniform mat4 projection;"
      , "uniform mat4 model;"
      , "in vec3 in_Position;"
      , "void main(void) {"
      , " gl_Position = projection * model * vec4(in_Position, 1.0);"
      , "}"
      ])
    GL.shaderSourceBS fragmentShader $= Text.encodeUtf8
      (Text.pack $ unlines
      [ "#version 130"
      , "out vec4 fragColor;"
      , "void main(void) {"
      , " fragColor = vec4(1.0,1.0,1.0,1.0);"
      , "}"
      ])
    GL.compileShader vertexShader
    GL.compileShader fragmentShader
    shaderProg <- GL.createProgram
    GL.attachShader shaderProg vertexShader
    GL.attachShader shaderProg fragmentShader
    GL.attribLocation shaderProg "in_Position" $= shaderAttribute
    GL.linkProgram shaderProg
    GL.currentProgram $= Just shaderProg

    let fov = 90
        s = recip (tan $ fov * 0.5 * pi / 180)
        f = 1000
        n = 1
    let perspective = V.fromList [ s, 0, 0, 0
                                 , 0, s, 0, 0
                                 , 0, 0, -(f/(f - n)), -1
                                 , 0, 0, -((f*n)/(f-n)), 1
                                 ]
    GL.UniformLocation loc <- GL.get (GL.uniformLocation shaderProg "projection")
    V.unsafeWith perspective $ \ptr -> GL.glUniformMatrix4fv loc 1 0 ptr

    forever $ do
      GL.clearColor $= GL.Color4 0.5 0.2 1 1
      GL.clear [GL.ColorBuffer]
      GL.UniformLocation loc <- GL.get (GL.uniformLocation shaderProg "model")
      with (distribute triangleTranslation) $ \ptr ->
        GL.glUniformMatrix4fv loc 1 0 (castPtr (ptr :: Ptr (M44 CFloat)))
      GL.drawArrays GL.Triangles 0 3
      SDL.glSwapWindow win
