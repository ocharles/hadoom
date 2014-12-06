module Light where

import Foreign (Storable(..), castPtr, nullPtr, plusPtr)
import Foreign.C (CFloat)
import Graphics.GL
import Linear as L
import Material
import Util

data LightShape
  = Omni
  | Spotlight (V3 CFloat) -- direction
              CFloat      -- full radius
              CFloat      -- penumbra radius
              (M33 CFloat) -- rotation matrix
  deriving (Show)

data Light =
  Light {lightPos :: V3 CFloat
        ,lightColor :: V3 CFloat
        ,lightRadius :: CFloat
        ,lightShape :: LightShape}
  deriving (Show)

instance Storable Light where
  sizeOf _ =
    sizeOf (undefined :: V4 CFloat) *
    4
  alignment _ = sizeOf (undefined :: V4 CFloat)
  peek _ = error "peek Light"
  poke ptr (Light pos col r shape) =
    do poke (castPtr ptr) pos
       poke (castPtr (ptr `plusPtr`
                      fromIntegral (sizeOf (undefined :: V4 CFloat))))
            col
       poke (castPtr (ptr `plusPtr`
                      fromIntegral
                        (sizeOf (undefined :: V4 CFloat) +
                         sizeOf (undefined :: V3 CFloat))))
            r
       let (dir,x,y) =
             case shape of
               Spotlight a b c _ -> (a,b,c)
               Omni -> (0,0,0)
       poke (castPtr (ptr `plusPtr`
                      fromIntegral
                        (sizeOf (undefined :: V4 CFloat) *
                         2)))
            dir
       poke (castPtr (ptr `plusPtr`
                      fromIntegral
                        (sizeOf (undefined :: V4 CFloat) *
                         2 +
                         sizeOf (undefined :: V3 CFloat))))
            x
       poke (castPtr (ptr `plusPtr`
                      fromIntegral
                        (sizeOf (undefined :: V4 CFloat) *
                         3)))
            y

shadowMapResolution :: GLsizei
shadowMapResolution = 1024

genLightDepthMap :: IO GLTextureObject
genLightDepthMap =
  do lightDepthMap <- overPtr (glGenTextures 1)
     glBindTexture GL_TEXTURE_2D lightDepthMap
     glTexParameteri GL_TEXTURE_2D GL_TEXTURE_COMPARE_MODE GL_COMPARE_REF_TO_TEXTURE
     glTexParameteri GL_TEXTURE_2D GL_TEXTURE_COMPARE_FUNC GL_LEQUAL
     glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST
     glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
     -- glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP
     -- GL.textureWrapMode GL.Texture2D GL.S $=
     --   (GL.Repeated,GL.Clamp)
     glTexImage2D GL_TEXTURE_2D
                  0
                  GL_DEPTH_COMPONENT24
                  shadowMapResolution
                  shadowMapResolution
                  0
                  GL_DEPTH_COMPONENT
                  GL_FLOAT
                  nullPtr
     return (GLTextureObject lightDepthMap)

newtype GLFramebufferObject =
  GLFramebufferObject {unGLFramebufferObject :: GLuint}

genLightFramebufferObject :: IO GLFramebufferObject
genLightFramebufferObject =
  do lightFBO <- overPtr (glGenFramebuffers 1)
     glBindFramebuffer GL_FRAMEBUFFER lightFBO
     glDrawBuffer GL_NONE
     return (GLFramebufferObject lightFBO)
