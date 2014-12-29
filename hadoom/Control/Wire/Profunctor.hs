{-# LANGUAGE GADTs #-}
module Control.Wire.Profunctor () where

import Control.Monad
import Control.Wire
import Data.Profunctor
import Prelude hiding ((.), id)

instance (Monad m) => Profunctor (Wire s e m) where
    dimap f g (WArr h)    = WArr (fmap g . h . fmap f)
    dimap _ g (WConst mx) = WConst (fmap g mx)
    dimap f g (WGen h)    = WGen (\ds -> liftM (fmap g ***! dimap f g) . h ds . fmap f)
    dimap f g WId         = WArr (fmap (g . f))
    dimap f g (WPure h)   = WPure (\ds -> (fmap g ***! dimap f g) . h ds . fmap f)

    lmap f (WArr g)       = WArr (g . fmap f)
    lmap _ (WConst mx)    = WConst mx
    lmap f (WGen g)       = WGen (\ds -> liftM (fmap (lmap f)) . g ds . fmap f)
    lmap f WId            = WArr (fmap f)
    lmap f (WPure g)      = WPure (\ds -> fmap (lmap f) . g ds . fmap f)

    rmap = fmap
