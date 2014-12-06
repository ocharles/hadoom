{-# LANGUAGE RecordWildCards #-}
module Shader where

import Prelude hiding (any, floor, ceiling)

import Control.Monad (when)
import Foreign
import Foreign.C.String
import Graphics.GL
import Util

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text

import Paths_hadoom

newtype GLProgram =
  GLProgram {unGLProgram :: GLuint}

createShaderProgram :: FilePath -> FilePath -> IO GLProgram
createShaderProgram vertexShaderPath fragmentShaderPath =
  do vertexShader <- glCreateShader GL_VERTEX_SHADER
     compileShader vertexShaderPath vertexShader
     fragmentShader <- glCreateShader GL_FRAGMENT_SHADER
     compileShader fragmentShaderPath fragmentShader
     shaderProg <- glCreateProgram
     glAttachShader shaderProg vertexShader
     glAttachShader shaderProg fragmentShader
     mapM_ (\(x,y) ->
              withCString x
                          (glBindAttribLocation shaderProg y))
           [("in_Position",positionAttribute)
           ,("in_Normal",normalAttribute)
           ,("in_Tangent",tangentAttribute)
           ,("in_Bitangent",bitangentAttribute)
           ,("in_UV",uvAttribute)]
     glLinkProgram shaderProg
     linked <- overPtr (glGetProgramiv shaderProg GL_LINK_STATUS)
     when (linked == GL_FALSE)
          (do maxLength <- overPtr (glGetProgramiv shaderProg GL_INFO_LOG_LENGTH)
              logLines <- allocaArray
                            (fromIntegral maxLength)
                            (\p ->
                               alloca (\lenP ->
                                         do glGetProgramInfoLog shaderProg maxLength lenP p
                                            len <- peek lenP
                                            peekCStringLen (p,fromIntegral len)))
              putStrLn logLines)
     return (GLProgram shaderProg)
  where compileShader path shader =
          do src <- getDataFileName path >>= Text.readFile
             BS.useAsCString
               (Text.encodeUtf8 src)
               (\ptr ->
                  withArray [ptr]
                            (\srcs ->
                               glShaderSource shader 1 srcs nullPtr))
             glCompileShader shader -- GL.get (GL.shaderInfoLog shader) >>= putStrLn

positionAttribute, uvAttribute, normalAttribute, tangentAttribute, bitangentAttribute :: GLuint
positionAttribute = 0
normalAttribute = 1
tangentAttribute = 2
bitangentAttribute = 3
uvAttribute = 4
