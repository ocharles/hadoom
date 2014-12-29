{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
module Physics where

import Control.Lens hiding (indices)
import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Control.Wire
import Data.Bool
import FRP.Netwire.Move
import Foreign.C (CFloat)
import Hadoom.Camera
import Light
import Linear as L
import Prelude hiding (any, floor, ceiling, (.), id)
import qualified Data.Vector as V
import qualified SDL

data Scene =
  Scene {sceneCamera :: M44 CFloat
        ,sceneLights :: V.Vector Light}

lightDir :: CFloat -> V3 CFloat
lightDir theta =
  case inv33 (fromQuaternion (axisAngle (V3 0 1 0) theta)) of
    Nothing -> error "lightDir: Unable to invert rotation matrix (the impossible happened)"
    Just m ->
      m !*
      V3 0 0 (-1) :: V3 CFloat

scene :: (Applicative m, HasTime t s, Monoid e, MonadFix m) => Wire s e m [SDL.Event] Scene
scene =
  Scene <$> worldCamera <*>
  (time <&>
   \t ->
     [Light (V3 0 2 0)
            (V3 1 1 1)
            50
            (Spotlight (lightDir (realToFrac (pi / 4 :: Float)))
                       0.8
                       0.1
                       (fromQuaternion
                          (axisAngle (V3 0 1 0)
                                     (realToFrac (pi / 4 :: Float)))))
     ,Light (V3 0 2 0)
            (V3 1 1 1)
            350
            (Spotlight (lightDir (realToFrac (-t)))
                       0.8
                       0.1
                       (fromQuaternion
                          (axisAngle (V3 0 1 0)
                                     (realToFrac (-t)))))
     ,Light (V3 0 2 3) 1 30 Omni
     ,Light (V3 (-3) 2 0) 1 30 Omni
     ,Light (V3 3 2 0) 1 30 Omni
     ,Light (V3 0 2 (-3)) 1 30 Omni])

worldCamera :: (Applicative m, HasTime t s, MonadFix m, Monoid e) => Wire s e m [SDL.Event] (M44 CFloat)
worldCamera =
  let c = camera
      forwardV = cameraForward <$> c
      strafeV = (fromQuaternion (axisAngle (V3 0 1 0) (pi / 2)) !*) <$> forwardV
      forwardSpeed = (+) <$> (bool 0 10 <$> keyHeld SDL.ScancodeW)
                         <*> (bool 0 (-10) <$> keyHeld SDL.ScancodeS)
      forwardMotion = (^*) <$> forwardV <*> forwardSpeed
      strafeSpeed = (+) <$> (bool 0 3 <$> keyHeld SDL.ScancodeA)
                        <*> (bool 0 (-3) <$> keyHeld SDL.ScancodeD)
      strafeMotion = (^*) <$> strafeV <*> strafeSpeed
  in set translation <$> ((+) <$> integral 0 . forwardMotion
                              <*> integral 0 . strafeMotion)
                      <*> (m33_to_m44 . fromQuaternion . cameraQuat <$> c)

keyPressed :: (Applicative m,Monoid e,MonadFix m)
           => SDL.Scancode -> Wire s e m [SDL.Event] (Event ())
keyPressed scancode =
  void <$>
  became (\events ->
            filter (\case
                      SDL.Event _ (SDL.KeyboardEvent{..}) -> keyboardEventKeyMotion == SDL.KeyDown
                      _ -> False)
                   events `hasScancode`
            scancode)

keyReleased :: (Applicative m,Monoid e, MonadFix m)
            => SDL.Scancode -> Wire s e m [SDL.Event] (Event ())
keyReleased scancode =
  void <$>
  became (\events ->
            filter (\case
                      SDL.Event _ (SDL.KeyboardEvent{..}) -> keyboardEventKeyMotion == SDL.KeyUp
                      _ -> False)
                   events `hasScancode`
            scancode)

keyHeld :: (Applicative m,Monoid e, MonadFix m)
        => SDL.Scancode -> Wire s e m [SDL.Event] Bool
keyHeld scancode =
  between . ((,,) <$> pure True <*> keyPressed scancode <*> keyReleased scancode) <|> pure False

hasScancode :: [SDL.Event] -> SDL.Scancode -> Bool
events `hasScancode` s =
  case events of
    (SDL.Event _ (SDL.KeyboardEvent{..})):xs -> SDL.keysymScancode keyboardEventKeysym ==
                                                  s || xs `hasScancode` s
    _:xs -> xs `hasScancode` s
    [] -> False
