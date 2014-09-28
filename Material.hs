{-# LANGUAGE RecordWildCards #-}
module Material where

import Prelude hiding (any, floor, ceiling, (.), id)

import Control.Category
import Graphics.Rendering.OpenGL (($=))

import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JP
import qualified Data.Vector.Storable as SV
import qualified Graphics.Rendering.OpenGL as GL

data Material =
  Material {matDiffuse :: GL.TextureObject
           ,matNormalMap :: GL.TextureObject}

data ColorSpace = SRGB | Linear

loadTexture :: FilePath -> ColorSpace -> IO GL.TextureObject
loadTexture path colorSpace =
  do x <- JP.readImage path
     case x of
       Right (JP.ImageYCbCr8 img) ->
         do t <- GL.genObjectName
            GL.textureBinding GL.Texture2D $=
              Just t
            GL.textureFilter GL.Texture2D $=
              ((GL.Linear',Just GL.Linear'),GL.Linear')
            let toRgb8 =
                  JP.convertPixel :: JP.PixelYCbCr8 -> JP.PixelRGB8
            case JP.pixelMap toRgb8
                             img of
              JP.Image w h d ->
                do SV.unsafeWith d $
                     \ptr ->
                       GL.texImage2D
                         GL.Texture2D
                         GL.NoProxy
                         0
                         (case colorSpace of SRGB -> GL.SRGB ; Linear -> GL.RGB32F)
                         (GL.TextureSize2D (fromIntegral w)
                                           (fromIntegral h))
                         0
                         (GL.PixelData GL.RGB GL.UnsignedByte ptr)
                   GL.generateMipmap' GL.Texture2D
                   GL.textureMaxAnisotropy GL.Texture2D $=
                     16
                   return t
       Left e -> error e
       _ -> error "Unknown image format"

activateMaterial :: Material -> IO ()
activateMaterial Material{..} =
  do GL.activeTexture $=
       GL.TextureUnit 0
     GL.textureBinding GL.Texture2D $=
       Just matDiffuse
     GL.activeTexture $=
       GL.TextureUnit 2
     GL.textureBinding GL.Texture2D $=
       Just matNormalMap
