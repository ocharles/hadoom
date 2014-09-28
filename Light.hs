module Light where

import Foreign (Storable(..), castPtr, nullPtr, plusPtr)
import Foreign.C (CFloat)
import Graphics.Rendering.OpenGL (($=))
import Linear as L

import qualified Graphics.Rendering.OpenGL as GL

data Light =
  Light {lightPos :: V3 CFloat
        ,lightColor :: V3 CFloat
        ,lightDirection :: Quaternion CFloat
        ,lightRadius :: CFloat}
  deriving (Show)

instance Storable Light where
  sizeOf _ =
    sizeOf (undefined :: V4 CFloat) *
    3
  alignment _ = sizeOf (undefined :: V4 CFloat)
  peek ptr = error "peek Light"
  poke ptr (Light pos col dir r) =
    do poke (castPtr ptr) pos
       poke (castPtr $ ptr `plusPtr`
             fromIntegral (sizeOf (undefined :: V4 CFloat)))
            col
       poke (castPtr $ ptr `plusPtr`
             fromIntegral
               (sizeOf (undefined :: V4 CFloat) *
                2))
            (case (inv33 (fromQuaternion dir)) of
               Just m ->
                 m !*
                 V3 0 0 (-1) :: V3 CFloat)
       poke (castPtr $ ptr `plusPtr`
             fromIntegral
               (sizeOf (undefined :: V4 CFloat) *
                2 +
                sizeOf (undefined :: V3 CFloat)))
            r

shadowMapResolution = 1024


genLightDepthMap :: IO GL.TextureObject
genLightDepthMap =
  do lightDepthMap <- GL.genObjectName
     GL.textureBinding GL.Texture2D $=
       Just lightDepthMap
     GL.textureCompareMode GL.Texture2D $=
       Just GL.Lequal
     GL.textureFilter GL.Texture2D $=
       ((GL.Nearest,Nothing),GL.Nearest)
     GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.Clamp)
     GL.texImage2D GL.Texture2D
                   GL.NoProxy
                   0
                   GL.DepthComponent16
                   (GL.TextureSize2D shadowMapResolution shadowMapResolution)
                   0
                   (GL.PixelData GL.DepthComponent GL.Float nullPtr)
     return lightDepthMap

genLightFramebufferObject :: IO GL.FramebufferObject
genLightFramebufferObject =
  do lightFBO <- GL.genObjectName
     GL.bindFramebuffer GL.Framebuffer $=
       lightFBO
     GL.drawBuffer $= GL.NoBuffers
     return lightFBO
