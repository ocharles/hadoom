module Hadoom.GL.Vertex where

import Control.Applicative
import Foreign (Storable(..), castPtr, nullPtr, plusPtr)
import Foreign.C (CFloat)
import Graphics.GL
import Linear as L hiding (outer)
import Prelude hiding (any, floor, ceiling, (.), id)
import Shader

data Vertex =
  Vertex {vPos :: {-# UNPACK #-} !(V3 CFloat)
         ,vNorm :: {-# UNPACK #-} !(V3 CFloat)
         ,vTangent :: {-# UNPACK #-} !(V3 CFloat)
         ,vBitangent :: {-# UNPACK #-} !(V3 CFloat)
         ,vUV :: {-# UNPACK #-} !(V2 CFloat)}
  deriving (Show)

instance Storable Vertex where
  sizeOf ~(Vertex p n t bn uv) = sizeOf p + sizeOf n + sizeOf t + sizeOf bn +
                                 sizeOf uv
  alignment _ = 0
  peek ptr =
    Vertex <$>
    peek (castPtr ptr) <*>
    peek (castPtr (ptr `plusPtr`
                   sizeOf (vPos undefined))) <*>
    peek (castPtr (ptr `plusPtr`
                   sizeOf (vPos undefined) `plusPtr`
                   sizeOf (vNorm undefined))) <*>
    peek (castPtr (ptr `plusPtr`
                   sizeOf (vPos undefined) `plusPtr`
                   sizeOf (vNorm undefined) `plusPtr`
                   sizeOf (vTangent undefined))) <*>
    peek (castPtr (ptr `plusPtr`
                   sizeOf (vPos undefined) `plusPtr`
                   sizeOf (vNorm undefined) `plusPtr`
                   sizeOf (vTangent undefined) `plusPtr`
                   sizeOf (vBitangent undefined)))
  poke ptr (Vertex p n t bn uv) =
    do poke (castPtr ptr) p
       poke (castPtr (ptr `plusPtr` sizeOf p)) n
       poke (castPtr (ptr `plusPtr` sizeOf p `plusPtr` sizeOf n)) t
       poke (castPtr (ptr `plusPtr` sizeOf p `plusPtr` sizeOf n `plusPtr`
                      sizeOf t))
            bn
       poke (castPtr (ptr `plusPtr` sizeOf p `plusPtr` sizeOf n `plusPtr`
                      sizeOf t `plusPtr` sizeOf bn))
            uv

configureVertexAttributes :: IO ()
configureVertexAttributes =
  do let stride =
           fromIntegral (sizeOf (undefined :: Vertex))
         normalOffset =
           fromIntegral (sizeOf (0 :: V3 CFloat))
         tangentOffset =
           normalOffset +
           fromIntegral (sizeOf (0 :: V3 CFloat))
         bitangentOffset =
           tangentOffset +
           fromIntegral (sizeOf (0 :: V3 CFloat))
         uvOffset =
           bitangentOffset +
           fromIntegral (sizeOf (0 :: V3 CFloat))
     glVertexAttribPointer positionAttribute 3 GL_FLOAT GL_FALSE stride nullPtr
     glEnableVertexAttribArray positionAttribute
     glVertexAttribPointer normalAttribute
                           3
                           GL_FLOAT
                           GL_FALSE
                           stride
                           (nullPtr `plusPtr` normalOffset)
     glEnableVertexAttribArray normalAttribute
     glVertexAttribPointer tangentAttribute
                           3
                           GL_FLOAT
                           GL_FALSE
                           stride
                           (nullPtr `plusPtr` tangentOffset)
     glEnableVertexAttribArray tangentAttribute
     glVertexAttribPointer bitangentAttribute
                           3
                           GL_FLOAT
                           GL_FALSE
                           stride
                           (nullPtr `plusPtr` bitangentOffset)
     glEnableVertexAttribArray bitangentAttribute
     glVertexAttribPointer uvAttribute
                           2
                           GL_FLOAT
                           GL_FALSE
                           stride
                           (nullPtr `plusPtr` uvOffset)
     glEnableVertexAttribArray uvAttribute
