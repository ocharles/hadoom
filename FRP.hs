{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module FRP where

import Prelude hiding ((.), id)

import Debug.Trace
import Control.Arrow
import Control.Monad.Fix
import Control.Applicative
import Control.Category
import Data.Functor.Compose
import Data.Profunctor

type Time = Double

--------------------------------------------------------------------------------
data Out m a b = Out { outVal :: b
                     , outThen :: (Wire m a b)
                     }

instance Functor m => Functor (Out m a) where
  fmap f (Out x w) = Out (f x) (fmap f w)

instance Applicative m => Applicative (Out m a) where
  pure a = Out a (pure a)
  (Out f w) <*> (Out x w') = Out (f x) (w <*> w')

instance Functor m => Profunctor (Out m) where
  lmap f (Out x w) = Out x (lmap f w)
  rmap f (Out x w) = Out (f x) (rmap f w)

--------------------------------------------------------------------------------
data Wire m a b = Wire (Time -> a -> m (Out m a b))

stepWire :: Time -> a -> Wire m a b -> m (Out m a b)
stepWire t a (Wire w) = w t a

instance Functor m => Functor (Wire m a) where
  fmap f (Wire w) = Wire $ \t a -> fmap (fmap f) (w t a)

instance Applicative m => Applicative (Wire m a) where
  pure a = Wire (\_ _ -> pure (pure a))
  Wire f <*> Wire x = Wire $ \t a ->
    getCompose $ Compose (f t a) <*> Compose (x t a)

instance (Applicative m, Monad m) => Category (Wire m) where
  id = Wire $ \_ a -> pure (Out a id)
  Wire f . Wire g = Wire $ \t a -> do
    Out b g' <- g t a
    Out c f' <- f t b
    return (Out c (f' . g'))

instance Functor m => Profunctor (Wire m) where
  rmap = fmap
  lmap f (Wire w) = Wire $ \t a -> lmap f <$> (w t (f a))

instance (Applicative m, Monad m) => Strong (Wire m) where
  first' (Wire w) = Wire $ \t (a, c) -> do
    Out b w' <- w t a
    return (Out (b, c) (first w'))

instance (Applicative m, Monad m) => Choice (Wire m) where
  left' ~(Wire w) = Wire $ \t e -> case e of
    Left a -> do
      Out x w' <- w t a
      return (Out (Left x) (left' w'))
    Right c -> return (Out (Right c) (left' (Wire w)))

instance (Applicative m, Monad m) => Arrow (Wire m) where
  arr f = f <$> id
  first = first'

instance (Applicative m, Monad m) => ArrowChoice (Wire m) where
  left = left'

instance (Applicative m, MonadFix m) => ArrowLoop (Wire m) where
  loop ~(Wire w) = Wire $ \t a ->
    fmap (\(Out v w') -> Out (fst v) (loop w')) $
      mfix $ \x -> w t (a, snd (outVal x))

--------------------------------------------------------------------------------
delay :: Applicative m => a -> Wire m a a
delay initial = Wire $ \_ a -> pure (Out initial (delay a))

time :: Applicative m => Wire m a Time
time = go 0 where go x = Wire $ \t _ -> pure (Out (t + x) (go (t + x)))

integral :: (Applicative m, MonadFix m, Fractional a) => Wire m a a
integral = proc x -> do
  dt <- (-) <$> time <*> delay 0 . time -< ()
  rec i <- delay 0 -< i + x * realToFrac dt
  returnA -< i

integralWhenNaive :: (Applicative m, MonadFix m, Fractional a) => Wire m (a, Bool) a
integralWhenNaive = proc (i,b) -> do
  v <- integral -< i
  vprev <- delay 0 -< v
  let vdelta = v - vprev
  rec result <- delay 0 -< if b then result + vdelta else result
  returnA -< result

integralWhenChoice :: (Applicative m, MonadFix m, Fractional a) => Wire m (a, Bool) a
integralWhenChoice = proc (i, b) -> do
  b' <- delay True -< b
  rec v <- if b' then integral -< i
                else returnA -< v
  returnA -< v
