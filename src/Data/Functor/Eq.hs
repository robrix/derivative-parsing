module Data.Functor.Eq where

class Functor f => EqF f where
  eqF :: (r -> r -> Bool) -> f r -> f r -> Bool
