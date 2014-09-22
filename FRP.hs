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
  {-# INLINE fmap #-}

instance Applicative m => Applicative (Out m a) where
  pure a = Out a (pure a)
  {-# INLINE pure #-}
  (Out f w) <*> (Out x w') = Out (f x) (w <*> w')
  {-# INLINE (<*>) #-}

instance Functor m => Profunctor (Out m) where
  lmap f (Out x w) = Out x (lmap f w)
  {-# INLINE lmap #-}
  rmap f (Out x w) = Out (f x) (rmap f w)
  {-# INLINE rmap #-}

--------------------------------------------------------------------------------
data Wire m a b = Wire (Time -> a -> m (Out m a b))

instance (Applicative m, Num b) => Num (Wire m a b) where
  (+) = liftA2 (+)

stepWire :: Time -> a -> Wire m a b -> m (Out m a b)
stepWire t a (Wire w) = w t a

instance Functor m => Functor (Wire m a) where
  fmap f (Wire w) = Wire $ \t a -> fmap (fmap f) (w t a)
  {-# INLINE fmap #-}

instance Applicative m => Applicative (Wire m a) where
  pure a = Wire (\_ _ -> pure (pure a))
  {-# INLINE pure #-}
  Wire f <*> Wire x = Wire $ \t a ->
    getCompose $ Compose (f t a) <*> Compose (x t a)
  {-# INLINE (<*>) #-}

instance (Applicative m, Monad m) => Category (Wire m) where
  id = Wire $ \_ a -> pure (Out a id)
  {-# INLINE id #-}
  Wire f . Wire g = Wire $ \t a -> do
    Out b g' <- g t a
    Out c f' <- f t b
    return (Out c (f' . g'))
  {-# INLINE (.) #-}

instance Functor m => Profunctor (Wire m) where
  rmap = fmap
  {-# INLINE rmap #-}
  lmap f (Wire w) = Wire $ \t a -> lmap f <$> (w t (f a))
  {-# INLINE lmap #-}

instance (Applicative m, Monad m) => Strong (Wire m) where
  first' (Wire w) = Wire $ \t (a, c) -> do
    Out b w' <- w t a
    return (Out (b, c) (first w'))
  {-# INLINE first' #-}

instance (Applicative m, Monad m) => Choice (Wire m) where
  left' ~(Wire w) = Wire $ \t e -> case e of
    Left a -> do
      Out x w' <- w t a
      return (Out (Left x) (left' w'))
    Right c -> return (Out (Right c) (left' (Wire w)))
  {-# INLINE left' #-}

instance (Applicative m, Monad m) => Arrow (Wire m) where
  arr f = f <$> id
  {-# INLINE arr #-}
  first = first'
  {-# INLINE first #-}

instance (Applicative m, Monad m) => ArrowChoice (Wire m) where
  left = left'
  {-# INLINE left #-}

instance (Applicative m, MonadFix m) => ArrowLoop (Wire m) where
  loop ~(Wire w) = Wire $ \t a ->
    fmap (\(Out v w') -> Out (fst v) (loop w')) $
      mfix $ \x -> w t (a, snd (outVal x))
  {-# INLINE loop #-}

--------------------------------------------------------------------------------
delay :: Applicative m => a -> Wire m a a
delay initial = Wire $ \_ a -> pure (Out initial (delay a))

time :: Applicative m => Wire m a Time
time = go 0 where go x = Wire $ \t _ -> pure (Out (t + x) (go (t + x)))

integral :: (Applicative m, MonadFix m, Fractional a, Show a) => Wire m a a
integral = proc x -> do
  dt <- (-) <$> time <*> delay 0 . time -< ()
  rec i <- delay 0 -< i + x * realToFrac dt
  returnA -< traceShowId i

integralWhen :: (Applicative m, MonadFix m, Fractional a, Show a) => Wire m (a, Bool) a
integralWhen = proc (i, b) -> do
  rec v' <- delay 0 -< v
      v <- if b then arr (traceShowId) . integral -< i
               else returnA -< v'
  returnA -< v
