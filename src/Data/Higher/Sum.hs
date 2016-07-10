{-# LANGUAGE PolyKinds, TypeOperators #-}
module Data.Higher.Sum where

import Data.Higher.Bifunctor
import Data.Higher.Functor

data (f :+: g) a
  = L (f a)
  | R (g a)

heither :: (f a -> b) -> (g a -> b) -> (f :+: g) a -> b
heither f g s = case s of
  L l -> f l
  R r -> g r


-- Instances

instance HBifunctor (:+:) where
  hbimap f g = heither (L . f) (R . g)

instance HFunctor ((:+:) f) where
  hfmap g = heither L (R . g)
