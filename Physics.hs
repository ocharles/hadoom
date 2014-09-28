{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedLists #-}
module Physics where

import Prelude hiding (any, floor, ceiling, (.), id)

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Lens hiding (indices)
import Control.Monad.Fix (MonadFix)
import Foreign.C (CFloat)
import Linear as L

import qualified Data.Vector as V
import qualified Graphics.UI.SDL.Enum as SDL
import qualified Graphics.UI.SDL.Types as SDL

import Light

import qualified FRP

data Scene =
  Scene {sceneCamera :: M44 CFloat
        ,sceneLights :: V.Vector Light}

scene :: FRP.Wire Identity [SDL.Event] Scene
scene =
  Scene <$> camera <*>
  (FRP.time <&>
   \t ->
     [Light (V3 0 0 0)
            (V3 1 0.5 0.5)
            (axisAngle (V3 0 1 0) $ pi + (pi / 8) * sin (realToFrac t))
            350
            Omni
     ,Light (V3 0 15 ((sin (realToFrac t) * 50 * 0.5 + 0.5) + 20))
            (V3 0.5 1 0.5)
            (axisAngle (V3 0 1 0) 0)
            350
            Spotlight])

camera :: FRP.Wire Identity [SDL.Event] (M44 CFloat)
camera = proc events -> do
  goForward <- keyHeld SDL.scancodeUp -< events
  goBack <- keyHeld SDL.scancodeDown -< events

  turnLeft <- keyHeld SDL.scancodeLeft -< events
  turnRight <- keyHeld SDL.scancodeRight -< events
  theta <- (FRP.integralWhen -< (-2, turnLeft)) + (FRP.integralWhen -< (2, turnRight))
  let quat = axisAngle (V3 0 1 0) theta

  rec position <- if goForward
                   then FRP.integral -< over _x negate $ rotate quat (V3 0 0 1) * 10
                   else returnA -< position'
      position' <- FRP.delay 0 -< position

  returnA -< m33_to_m44 (fromQuaternion quat) !*! mkTransformation 0 (position - V3 0 10 0)

keyPressed :: (Applicative m, MonadFix m) => SDL.Scancode -> FRP.Wire m [SDL.Event] Bool
keyPressed scancode = proc events -> do
  rec pressed <- FRP.delay False -<
                   pressed ||
                     (filter ((== SDL.eventTypeKeyDown) . SDL.eventType) events
                        `hasScancode` scancode)
  returnA -< pressed

keyReleased :: (Applicative m, MonadFix m) => SDL.Scancode -> FRP.Wire m [SDL.Event] Bool
keyReleased scancode =
  proc events ->
  do rec released <- FRP.delay False -<
                       released ||
                         (filter ((== SDL.eventTypeKeyUp) . SDL.eventType) events
                            `hasScancode` scancode)
     returnA -< released

keyHeld :: (Applicative m, MonadFix m) => SDL.Scancode -> FRP.Wire m [SDL.Event] Bool
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
    (SDL.KeyboardEvent _ _ _ _ _ (SDL.Keysym scancode _ _)) : xs -> scancode == s || xs `hasScancode` s
    _ : xs -> xs `hasScancode` s
    [] -> False
