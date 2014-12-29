{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Hadoom.Camera (Camera(..), camera, cameraQuat, cameraForward) where

import Data.Profunctor
import Control.Applicative
import Control.Category
import FRP.Netwire.Move (integral)
import Control.Lens (alaf)
import Control.Wire.Profunctor ()
import Data.Foldable
import Data.Monoid
import Linear
import Prelude hiding ((.), id)
import qualified SDL

import Control.Wire

data Camera a =
  Camera {cameraPitch :: a
         ,cameraYaw :: a}

-- | A camera in its own model space.
camera :: (Foldable f, Fractional a, HasTime t s, Monad m) => Wire s e m (f SDL.Event) (Camera a)
camera =
  dimap (alaf Sum foldMap (mouseMotion . SDL.eventPayload))
        (\(V2 (negate -> cameraYaw) (negate -> cameraPitch)) ->
           Camera {..})
        (integral 0)
  where mouseMotion ev =
          case ev of
            SDL.MouseMotionEvent{..} ->
              sensitivity *^
              (fromIntegral <$> mouseMotionEventRelMotion)
            _ -> 0
        sensitivity = 0.1

-- | Interpret a 'Camera' for its rotation quaternion.
cameraQuat :: (Epsilon a,RealFloat a) => Camera a -> Quaternion a
cameraQuat Camera{..} =
  axisAngle (V3 0 1 0) cameraYaw *
  axisAngle (V3 1 0 0) cameraPitch

-- | Determine the forward vector in world-space for a given 'Camera'.
cameraForward :: (Epsilon a, RealFloat a) => Camera a -> V3 a
cameraForward Camera{..} =
  fromQuaternion (axisAngle (V3 0 1 0) cameraYaw) !*
  V3 0 0 (-1)
