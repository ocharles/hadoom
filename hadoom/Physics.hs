{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
module Physics where

import BasePrelude
import Control.Lens hiding (indices)
import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Control.Wire hiding (loop)
import Foreign.C (CFloat)
import Hadoom.BSP
import Hadoom.Camera
import Hadoom.Geometry
import Hadoom.WorldBSP
import Light
import Linear as L
import TestWorld
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
  let cam = worldCamera
      lights = time <&> \t ->
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
          ,Light (V3 0 2 (-3)) 1 30 Omni]
  in Scene <$> cam <*> lights

worldCamera :: (Applicative m, HasTime t s, MonadFix m, Monoid e) => Wire s e m [SDL.Event] (M44 CFloat)
worldCamera =
  let c = camera
      speedFromKey s scancode = bool 0 s <$> keyHeld scancode
      forwardV = cameraForward <$> c
      strafeV = (fromQuaternion (axisAngle (V3 0 1 0) (pi / 2)) !*) <$> forwardV
      forwardSpeed =
        (+) <$>
        speedFromKey 10 SDL.ScancodeW <*>
        speedFromKey (-10) SDL.ScancodeS
      forwardMotion = (^*) <$> forwardV <*> forwardSpeed
      strafeSpeed =
        (+) <$>
        speedFromKey 3 SDL.ScancodeA <*>
        speedFromKey (-3) SDL.ScancodeD
      strafeMotion = (^*) <$> strafeV <*> strafeSpeed
      worldBSP = compileBSP testWorld
      translation2D = (+) <$> integralSatisfying noCollisions 0 . forwardMotion
                          <*> integralSatisfying noCollisions 0 . strafeMotion
      playerRadius = 0.4
      noCollisions p = not (circleIntersects worldBSP (Circle (realToFrac <$> p ^. _xz) playerRadius))
  in set translation
       <$> ((+) <$> pure (V3 0 1.75 0)
                <*> translation2D)
       <*> (m33_to_m44 . fromQuaternion . cameraQuat <$> c)

integralSatisfying :: (Fractional a,HasTime t s)
                   => (a -> Bool) -> a -> Wire s e m a a
integralSatisfying satisfies = loop
  where loop x' =
          mkPure $
          \ds dx ->
            let dt = realToFrac (dtime ds)
                x = x' + dt * dx
            in if satisfies x
                 then (Right x',loop x)
                 else (Right x',loop x')

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
    SDL.Event _ (SDL.KeyboardEvent{..}):xs -> SDL.keysymScancode keyboardEventKeysym == s || xs `hasScancode` s
    _:xs -> xs `hasScancode` s
    [] -> False
