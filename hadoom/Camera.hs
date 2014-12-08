{-# LANGUAGE RecordWildCards #-}
module Camera (cameraQuat) where

import Control.Category
import Prelude hiding ((.), id)
import Control.Applicative
import Control.Lens ((<&>))
import Data.Foldable
import Data.Functor.Identity
import Data.Monoid
import Foreign.C.Types
import Linear
import qualified FRP
import qualified SDL

cameraQuat :: FRP.Wire Identity [SDL.Event] (Quaternion CFloat)
cameraQuat =
  (id <&>
   \(V2 yaw pitch) ->
     axisAngle (V3 1 0 0) pitch *
     axisAngle (V3 0 1 0) yaw) .
  FRP.integral .
  (getSum . foldMap mouseMotion . map SDL.eventPayload <$> id)
  where mouseMotion ev =
          case ev of
            SDL.MouseMotionEvent{..} ->
              Sum (sensitivity *^ (fromIntegral <$> mouseMotionEventRelMotion))
            _ -> mempty
        sensitivity = 0.1
