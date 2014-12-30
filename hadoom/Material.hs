{-# LANGUAGE RecordWildCards #-}
module Material where

import Control.Monad (when)
import Foreign.Ptr
import Graphics.GL
import Graphics.GL.Ext.EXT.TextureFilterAnisotropic
import Prelude hiding (any, ceiling)
import Util
import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JP
import qualified Data.Vector.Storable as SV

newtype GLTextureObject =
  GLTextureObject {unGLTextureObject :: GLuint}

data Material =
  Material {matDiffuse :: GLTextureObject
           ,matNormalMap :: GLTextureObject}

data ColorSpace
  = SRGB
  | Linear

loadTexture :: FilePath -> ColorSpace -> IO GLTextureObject
loadTexture path colorSpace =
  do x <- JP.readImage path
     case x of
       Right dimg ->
         do t <- overPtr (glGenTextures 1)
            glBindTexture GL_TEXTURE_2D t
            glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR_MIPMAP_LINEAR
            glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR
            glPixelStorei GL_UNPACK_LSB_FIRST 0
            glPixelStorei GL_UNPACK_SWAP_BYTES 0
            glPixelStorei GL_UNPACK_ROW_LENGTH 0
            glPixelStorei GL_UNPACK_IMAGE_HEIGHT 0
            glPixelStorei GL_UNPACK_SKIP_ROWS 0
            glPixelStorei GL_UNPACK_SKIP_PIXELS 0
            glPixelStorei GL_UNPACK_SKIP_IMAGES 0
            glPixelStorei GL_UNPACK_ALIGNMENT 1
            let width =
                  fromIntegral (JP.dynamicMap JP.imageWidth dimg)
                height =
                  fromIntegral (JP.dynamicMap JP.imageHeight dimg)
            glTexStorage2D
              GL_TEXTURE_2D
              (floor (logBase (2 :: Float)
                              (fromIntegral (max width height))))
              (case colorSpace of
                 SRGB -> GL_SRGB8
                 Linear -> GL_RGB32F)
              width
              height
            case dimg of
              JP.ImageRGB8 (JP.Image _ _ d) ->
                SV.unsafeWith
                  d
                  (glTexSubImage2D GL_TEXTURE_2D 0 0 0 width height GL_RGB GL_UNSIGNED_BYTE .
                   castPtr)
              JP.ImageRGBA8 (JP.Image _ _ d) ->
                SV.unsafeWith
                  d
                  (glTexSubImage2D GL_TEXTURE_2D 0 0 0 width height GL_RGBA GL_UNSIGNED_BYTE .
                   castPtr)
              JP.ImageYCbCr8 img ->
                let toRgb8 =
                      JP.convertPixel :: JP.PixelYCbCr8 -> JP.PixelRGB8
                in case JP.pixelMap toRgb8 img of
                     JP.Image _ _ d ->
                       SV.unsafeWith
                         d
                         (glTexSubImage2D GL_TEXTURE_2D
                                          0
                                          0
                                          0
                                          width
                                          height
                                          GL_RGB
                                          GL_UNSIGNED_BYTE .
                          castPtr)
              _ -> error "Unknown image format"
            glGenerateMipmap GL_TEXTURE_2D
            when gl_EXT_texture_filter_anisotropic
                 (glTexParameterf GL_TEXTURE_2D GL_TEXTURE_MAX_ANISOTROPY_EXT 16)
            return (GLTextureObject t)
       Left e -> error e

activateMaterial :: Material -> IO ()
activateMaterial Material{..} =
  do glActiveTexture GL_TEXTURE0
     glBindTexture GL_TEXTURE_2D
                   (unGLTextureObject matDiffuse)
     glActiveTexture GL_TEXTURE2
     glBindTexture GL_TEXTURE_2D
                   (unGLTextureObject matNormalMap)
