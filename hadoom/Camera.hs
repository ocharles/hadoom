{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Camera (Camera(..), camera, cameraQuat, cameraForward) where

import Control.Arrow
import Control.Applicative
import Control.Category
import Control.Lens (alaf)
import Data.Foldable
import Data.Functor.Identity
import Data.Monoid
import Linear
import Prelude hiding ((.), id)
import qualified FRP
import qualified SDL

data Camera a =
  Camera {cameraPitch :: a
         ,cameraYaw :: a}

-- | A camera in its own model space.
camera :: (Fractional a) => FRP.Wire Identity [SDL.Event] (Camera a)
camera =
  arr (\(V2 (negate -> cameraYaw) (negate -> cameraPitch)) ->
         Camera {..}) .
  FRP.integral .
  arr (alaf Sum foldMap (mouseMotion . SDL.eventPayload))
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
