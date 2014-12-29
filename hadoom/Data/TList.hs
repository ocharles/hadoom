{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
module Data.TList (TList(..), tmap, ttraverse) where

import Control.Applicative

infixr 8 :::
data TList :: (k -> *) -> [k] -> * where
  TNil :: TList f '[]
  (:::) :: f t -> TList f ts -> TList f (t ': ts)

tmap :: (forall a. f a -> g a) -> TList f as -> TList g as
tmap _ TNil = TNil
tmap f (x ::: xs) = f x ::: tmap f xs

ttraverse :: Applicative i => (forall a. f a -> i (g a)) -> TList f xs -> i (TList g xs)
ttraverse _ TNil = pure TNil
ttraverse f (x ::: xs) = (:::) <$> f x <*> ttraverse f xs
