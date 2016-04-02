{-# LANGUAGE FlexibleInstances #-}
module Data.Functor.Eq where

class Functor f => EqF f where
  eqF :: (r -> r -> Bool) -> f r -> f r -> Bool

instance (EqF f, Eq a) => Eq (f a) where
  (==) = eqF (==)
