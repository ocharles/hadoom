{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module FRP where

import Prelude hiding ((.), id)

import Control.Arrow
import Control.Monad
import Control.Monad.Fix
import Control.Applicative
import Control.Category
import Data.Monoid
import Data.Profunctor

type Time = Double

--------------------------------------------------------------------------------
newtype Wire m a b = Wire { stepWire :: Time -> a -> m (b, Wire m a b) }

instance (MonadPlus m) => Alternative (Wire m a) where
    empty = Wire (\_ _ -> mzero)

    w1 <|> w2 =
        Wire $ \dt x' ->
            stepWire w1 dt x' `mplus`
            stepWire w2 dt x'

instance (Monad m) => Applicative (Wire m a) where
    pure x = let w = Wire (\_ _ -> return (x, w)) in w

    wf' <*> wx' =
        Wire $ \dt x' ->
            liftM2 (\(f, wf) (x, wx) -> (f x, wf <*> wx))
                   (stepWire wf' dt x')
                   (stepWire wx' dt x')

instance (Monad m) => Arrow (Wire m) where
    arr f = let w = Wire (\_ x -> return (f x, w)) in w
    first = first'
    second = second'

instance (MonadFix m) => ArrowLoop (Wire m) where
    loop w' =
        Wire $ \dt x' ->
            liftM (fst *** loop) $
            mfix (\r -> stepWire w' dt (x', snd (fst r)))

instance (MonadPlus m) => ArrowPlus (Wire m) where
    (<+>) = (<|>)

instance (MonadPlus m) => ArrowZero (Wire m) where
    zeroArrow = empty

instance (Monad m) => Category (Wire m) where
    id = Wire (\_ x -> return (x, id))

    w2' . w1' =
        Wire $ \dt x0 -> do
            (x1, w1) <- stepWire w1' dt x0
            (x2, w2) <- stepWire w2' dt x1
            return (x2, w2 . w1)

instance (Monad m) => Functor (Wire m a) where
    fmap = rmap

instance (Monad m, Floating b) => Floating (Wire m a b) where
    (**) = liftA2 (**)
    exp = fmap exp
    log = fmap log
    logBase = liftA2 logBase
    pi = pure pi
    sqrt = fmap sqrt

    sin = fmap sin; asin = fmap asin; sinh = fmap sinh; asinh = fmap asinh
    cos = fmap cos; acos = fmap acos; cosh = fmap cosh; acosh = fmap acosh
    tan = fmap tan; atan = fmap atan; tanh = fmap tanh; atanh = fmap atanh

instance (Monad m, Fractional b) => Fractional (Wire m a b) where
    (/) = liftA2 (/)
    recip = fmap recip
    fromRational = pure . fromRational

instance (Monad m, Monoid b) => Monoid (Wire m a b) where
    mempty = pure mempty
    mappend = liftA2 (<>)

instance (Monad m, Num b) => Num (Wire m a b) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    negate = fmap negate
    signum = fmap signum
    fromInteger = pure . fromInteger

instance (Monad m) => Profunctor (Wire m) where
    dimap lf rf = go
        where
        go w' = Wire (\dt -> liftM (rf *** go) . stepWire w' dt . lf)

instance (Monad m) => Strong (Wire m) where
    first' w' =
        Wire $ \dt (x', y) ->
            liftM ((, y) *** first') (stepWire w' dt x')

    second' w' =
        Wire $ \dt (x, y') ->
            liftM ((x,) *** second') (stepWire w' dt y')

instance (Applicative m, Monad m) => Choice (Wire m) where
  left' ~(Wire w) = Wire $ \t e -> case e of
    Left a -> do
      (x, w') <- w t a
      return (Left x, left' w')
    Right c -> return (Right c, left' (Wire w))
  {-# INLINE left' #-}

instance (Applicative m, Monad m) => ArrowChoice (Wire m) where
  left = left'

--------------------------------------------------------------------------------
delay :: Applicative m => a -> Wire m a a
delay initial =
  Wire (\_ a -> pure (initial, delay a))

time :: Applicative m => Wire m a Time
time = go 0
  where go x =
          Wire (\t _ -> pure (t + x,go (t + x)))

integral :: (Applicative m, MonadFix m, Fractional a, Show a) => Wire m a a
integral =
  proc x ->
  do dt <- (-) <$> time <*> delay 0 . time -< ()
     rec i <- delay 0 -< i + x * realToFrac dt
     returnA -< i

integralWhen :: (Applicative m, MonadFix m, Fractional a, Show a) => Wire m (a, Bool) a
integralWhen =
  proc (i, b) ->
  do rec v' <- delay 0 -< v
         v <- if b then integral -< i else returnA -< v'
     returnA -< v
