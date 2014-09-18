{-# LANGUAGE OverloadedLists #-}
module Main where

import Prelude hiding (floor, ceiling)

import Control.Lens hiding (indices)
import Data.Distributive (distribute)
import Data.Function (fix)
import Data.Int (Int32)
import Data.Monoid ((<>))
import Foreign.C (CFloat, withCString)
import Foreign (Ptr, alloca, castPtr, nullPtr, peek, plusPtr, sizeOf, with)
import Graphics.Rendering.OpenGL (($=))
import Linear as L

import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Vector.Storable as V
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Graphics.UI.SDL.Basic as SDL
import qualified Graphics.UI.SDL.Video as SDL

import Paths_hadoom

type Sector = V.Vector (V2 CFloat)

realiseSector :: Sector -> IO (IO ())
realiseSector sectorVertices = do
  vbo <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just vbo

  let expandEdge start@(V2 x1 y1) end@(V2 x2 y2) =
        let n = case normalize $ perp $ end ^-^ start of
                  V2 x y -> V3 x 0 y
        in concat $ zipWith (\a b -> [a, b])
                            [ V3 x1 (-10) y1
                            , V3 x1   10  y1
                            , V3 x2 (-10) y2
                            , V3 x2   10  y2
                            ]
                            (repeat n)

  let vertices = V.fromList $ concat $ zipWith expandEdge (V.toList sectorVertices)
                                                          (V.toList $ V.tail sectorVertices <> sectorVertices)

  V.unsafeWith vertices $ \verticesPtr ->
    GL.bufferData GL.ArrayBuffer $=
      (fromIntegral (V.length sectorVertices * 2 * 2 * 3 * 2 * sizeOf (0 :: CFloat)), verticesPtr, GL.StaticDraw)

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
    _ <- SDL.init 0x00000020
    _ <- SDL.createWindowAndRenderer 800 600 0 winPtr rendererPtr

    win <- peek winPtr

    withCString "Hadoom" $ SDL.setWindowTitle win

    GL.clearColor $= GL.Color4 0.5 0.5 0.5 1

    drawSector <- realiseSector [ V2 (-25) (-25), V2 0 (-40), V2 25 (-25), V2 25 25, V2 (-25) 25 ]

    let stride = fromIntegral $ sizeOf (0 :: V3 CFloat) * 2
        normalOffset = fromIntegral $ sizeOf (0 :: V3 CFloat)

    GL.vertexAttribPointer positionAttribute $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float stride nullPtr)
    GL.vertexAttribArray positionAttribute $= GL.Enabled

    GL.vertexAttribPointer normalAttribute $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float stride (nullPtr `plusPtr` normalOffset))
    GL.vertexAttribArray normalAttribute $= GL.Enabled

    shaderProg <- createShaderProgram "shaders/vertex/projection-model.glsl"
                                     "shaders/fragment/solid-white.glsl"
    GL.currentProgram $= Just shaderProg

    let perspective =
          let fov = 90
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

    (fix $ \f n -> do
       GL.clear [GL.ColorBuffer]

       with (distribute (triangleTranslation !*! (eye4 & translation .~ V3 0 0 n))) $ \ptr -> do
         GL.UniformLocation loc <- GL.get (GL.uniformLocation shaderProg "model")
         GL.glUniformMatrix4fv loc 1 0 (castPtr (ptr :: Ptr (M44 CFloat)))

       drawSector

       SDL.glSwapWindow win

       f (n + 0.001)) 0

positionAttribute :: GL.AttribLocation
positionAttribute = GL.AttribLocation 0

normalAttribute :: GL.AttribLocation
normalAttribute = GL.AttribLocation 1

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

  GL.linkProgram shaderProg

  return shaderProg

  where
  compileShader path shader = do
    src <- getDataFileName path >>= Text.readFile
    GL.shaderSourceBS shader $= Text.encodeUtf8 src
    GL.compileShader shader
