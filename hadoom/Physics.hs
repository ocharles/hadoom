{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
module Physics where

import Camera
import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Lens hiding (indices)
import Control.Monad.Fix (MonadFix)
import Foreign.C (CFloat)
import Light
import Linear as L
import Prelude hiding (any, floor, ceiling, (.), id)
import qualified Data.Vector as V
import qualified FRP
import qualified SDL

data Scene =
  Scene {sceneCamera :: M44 CFloat
        ,sceneLights :: V.Vector Light}

lightDir :: CFloat -> V3 CFloat
lightDir theta =
  case inv33 (fromQuaternion (axisAngle (V3 0 1 0) theta)) of
    Just m ->
      m !*
      V3 0 0 (-1) :: V3 CFloat

scene :: FRP.Wire Identity [SDL.Event] Scene
scene =
  Scene <$> worldCamera <*>
  (FRP.time <&>
   \t ->
     [Light (V3 0 2 0)
            (V3 1 1 1)
            50
            (Spotlight (lightDir (realToFrac (pi / 4)))
                       0.8
                       0.1
                       (fromQuaternion
                          (axisAngle (V3 0 1 0)
                                     (realToFrac (pi / 4)))))
     ,Light (V3 0 2 0)
            (V3 1 1 1)
            350
            (Spotlight (lightDir (realToFrac (-t)))
                       0.8
                       0.1
                       (fromQuaternion
                          (axisAngle (V3 0 1 0)
                                     (realToFrac (-t)))))
     ,Light (V3 0 2 3) 1 3 Omni
     ,Light (V3 (-3) 2 0) 1 3 Omni
     ,Light (V3 3 2 0) 1 3 Omni
     ,Light (V3 0 2 (-3)) 1 3 Omni
            ])
     -- ,Light (V3 0 2 0)
     --        (V3 0.5 0.5 1)
     --        10
     --        Omni])

worldCamera :: FRP.Wire Identity [SDL.Event] (M44 CFloat)
worldCamera =
  proc events ->
  do goForward <- keyHeld SDL.ScancodeW -< events
     goBackward <- keyHeld SDL.ScancodeS -< events
     strafeLeft <- keyHeld SDL.ScancodeA -< events
     strafeRight <- keyHeld SDL.ScancodeD -< events
     c <- camera -< events
     let forwardV = cameraForward c
         speed = 5
         strafeSpeed = 3
     rec forward <- if goForward then FRP.integral -< forwardV * speed else
                      returnA -< forward'
         forward' <- FRP.delay 0 -< forward
         backward <- if goBackward then
                       FRP.integral -< negate forwardV * speed else returnA -< backward'
         backward' <- FRP.delay 0 -< backward
         let strafeV
               = fromQuaternion (axisAngle (V3 0 1 0) (pi / 2)) !* forwardV
         strafeL <- if strafeLeft then FRP.integral -< strafeV * strafeSpeed else
                      returnA -< strafeL'
         strafeL' <- FRP.delay 0 -< strafeL
         strafeR <- if strafeRight then FRP.integral -< negate strafeV * strafeSpeed else
                      returnA -< strafeR'
         strafeR' <- FRP.delay 0 -< strafeR
     returnA -<
       m33_to_m44 (fromQuaternion (cameraQuat c)) & translation .~
         (forward + backward + strafeL + strafeR + V3 0 1.75 0)

keyPressed :: (Applicative m,MonadFix m)
           => SDL.Scancode -> FRP.Wire m [SDL.Event] Bool
keyPressed scancode =
  proc events ->
  do rec pressed <- FRP.delay False -<
                      pressed ||
                        (filter
                           (\case
                                SDL.Event _ (SDL.KeyboardEvent{..}) -> keyboardEventKeyMotion ==
                                                                         SDL.KeyDown
                                _ -> False)
                           events
                           `hasScancode` scancode)
     returnA -< pressed

keyReleased :: (Applicative m,MonadFix m)
            => SDL.Scancode -> FRP.Wire m [SDL.Event] Bool
keyReleased scancode =
  proc events ->
  do rec released <- FRP.delay False -<
                       released ||
                         (filter
                            (\case
                                 SDL.Event _ (SDL.KeyboardEvent{..}) -> keyboardEventKeyMotion ==
                                                                          SDL.KeyUp
                                 _ -> False)
                            events
                            `hasScancode` scancode)
     returnA -< released

keyHeld :: (Applicative m,MonadFix m)
        => SDL.Scancode -> FRP.Wire m [SDL.Event] Bool
keyHeld scancode =
  proc events ->
  do pressed <- keyPressed scancode -< events
     if pressed then
       do released <- keyReleased scancode -< events
          if released then FRP.delay False . keyHeld scancode -< events else
            returnA -< True
       else returnA -< False

hasScancode :: [SDL.Event] -> SDL.Scancode -> Bool
events `hasScancode` s =
  case events of
    (SDL.Event _ (SDL.KeyboardEvent{..})):xs -> SDL.keysymScancode keyboardEventKeysym ==
                                                  s || xs `hasScancode` s
    _:xs -> xs `hasScancode` s
    [] -> False
