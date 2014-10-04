{-# LANGUAGE RecordWildCards #-}
module Shader where

import Prelude hiding (any, floor, ceiling)

import Control.Monad (unless)
import Graphics.Rendering.OpenGL (($=))

import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Graphics.Rendering.OpenGL as GL

import Paths_hadoom

createShaderProgram :: FilePath -> FilePath -> IO GL.Program
createShaderProgram vertexShaderPath fragmentShaderPath =
  do vertexShader <- GL.createShader GL.VertexShader
     compileShader vertexShaderPath vertexShader
     fragmentShader <- GL.createShader GL.FragmentShader
     compileShader fragmentShaderPath fragmentShader
     shaderProg <- GL.createProgram
     GL.attachShader shaderProg vertexShader
     GL.attachShader shaderProg fragmentShader
     GL.attribLocation shaderProg "in_Position" $=
       positionAttribute
     GL.attribLocation shaderProg "in_Normal" $=
       normalAttribute
     GL.attribLocation shaderProg "in_Tangent" $=
       tangentAttribute
     GL.attribLocation shaderProg "in_Bitangent" $=
       bitangentAttribute
     GL.attribLocation shaderProg "in_UV" $=
       uvAttribute
     GL.linkProgram shaderProg
     linked <- GL.get (GL.linkStatus shaderProg)
     unless linked $ do
       GL.get (GL.programInfoLog shaderProg) >>= putStrLn
     return shaderProg
  where compileShader path shader =
          do src <- getDataFileName path >>= Text.readFile
             GL.shaderSourceBS shader $= Text.encodeUtf8 src
             GL.compileShader shader
             GL.get (GL.shaderInfoLog shader) >>=
               putStrLn

positionAttribute, uvAttribute, normalAttribute, tangentAttribute, bitangentAttribute :: GL.AttribLocation
positionAttribute = GL.AttribLocation 0
normalAttribute = GL.AttribLocation 1
tangentAttribute = GL.AttribLocation 2
bitangentAttribute = GL.AttribLocation 3
uvAttribute = GL.AttribLocation 4
