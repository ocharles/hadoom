{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
module Physics where

import Prelude hiding (any, floor, ceiling, (.), id)

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Lens hiding (indices)
import Control.Monad.Fix (MonadFix)
import Foreign.C (CFloat)
import Light
import Linear as L

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
  Scene <$> camera <*>
  (FRP.time <&>
   \t ->
     [Light (V3 0 10 0)
            (V3 1 1 1)
            1000
            (Spotlight (lightDir (realToFrac t))
                       0.8
                       0.1
                       (fromQuaternion
                          (axisAngle (V3 0 1 0)
                                     (realToFrac t))))
     ,Light (V3 0 15 0)
            (V3 1 1 1)
            350
            (Spotlight (lightDir (realToFrac (-t)))
                       0.8
                       0.1
                       (fromQuaternion
                          (axisAngle (V3 0 1 0)
                                     (realToFrac (-t)))))
     ,Light (V3 0 10 30) 1 30 Omni
     ,Light (V3 (-30) 10 0) 1 30 Omni
     ,Light (V3 30 10 0) 1 30 Omni
     ,Light (V3 0 10 (-30)) 1 30 Omni
     ,Light (V3 0 10 0)
            (V3 0.5 0.5 1)
            40
            Omni])

camera :: FRP.Wire Identity [SDL.Event] (M44 CFloat)
camera =
  proc events ->
  do goForward <- keyHeld SDL.ScancodeUp -< events
     goBack <- keyHeld SDL.ScancodeDown -< events
     turnLeft <- keyHeld SDL.ScancodeLeft -< events
     turnRight <- keyHeld SDL.ScancodeRight -< events
     theta <- (FRP.integralWhen -< (-2, turnLeft)) +
                (FRP.integralWhen -< (2, turnRight))
     let quat = axisAngle (V3 0 1 0) theta
     rec position <- if goForward then
                       FRP.integral -< over _x negate (rotate quat (V3 0 0 1) * 10) else
                       returnA -< position'
         position' <- FRP.delay 0 -< position
     returnA -<
       m33_to_m44 (fromQuaternion quat) !*!
         mkTransformation 0 (position - V3 0 10 0)

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
