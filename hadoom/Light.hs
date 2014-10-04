module Light where

import Foreign (Storable(..), allocaBytes, castPtr, nullPtr, plusPtr)
import Foreign.C (CFloat)
import Graphics.Rendering.OpenGL (($=))
import Linear as L

import qualified Graphics.Rendering.OpenGL as GL

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
  peek ptr = error "peek Light"
  poke ptr (Light pos col r shape) =
    do poke (castPtr ptr) pos
       poke (castPtr $ ptr `plusPtr` fromIntegral (sizeOf (undefined :: V4 CFloat)))
            col
       poke (castPtr $ ptr `plusPtr` fromIntegral (sizeOf (undefined :: V4 CFloat) + sizeOf (undefined :: V3 CFloat)))
            r

       let (dir, x, y ) = case shape of
             Spotlight a b c _ -> (a, b, c)
             Omni -> (0, 0, 0)

       poke (castPtr $ ptr `plusPtr` fromIntegral (sizeOf (undefined :: V4 CFloat) * 2))
            dir
       poke (castPtr $ ptr `plusPtr` fromIntegral (sizeOf (undefined :: V4 CFloat) * 2 + sizeOf (undefined :: V3 CFloat)))
            x
       poke (castPtr $ ptr `plusPtr` fromIntegral (sizeOf (undefined :: V4 CFloat) * 3))
            y

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
                   GL.DepthComponent24
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
