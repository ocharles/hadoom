{-# LANGUAGE RecordWildCards #-}
module Shader where

import Prelude hiding (any, floor, ceiling, (.), id)

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Lens hiding (indices)
import Control.Monad.Fix (MonadFix)
import Data.Distributive (distribute)
import Data.Foldable (any, for_)
import Data.Function (fix)
import Data.Int (Int32)
import Data.Monoid ((<>))
import Data.Time (getCurrentTime, diffUTCTime)
import Foreign (Ptr, Storable(..), alloca, castPtr, nullPtr, plusPtr, with)
import Foreign.C (CFloat, withCString)
import Graphics.Rendering.OpenGL (($=))
import Linear as L
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

import Graphics.Rendering.OpenGL (($=))

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
