module Data.Functor.Eq where

class Functor f => EqF f where
  eqF :: (r -> r -> Bool) -> f r -> f r -> Bool

instance EqF [] where
  eqF eq a b = and (zipWith eq a b) && length a == length b
