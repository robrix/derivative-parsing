module Data.Functor.Eq where

import Control.Applicative

class Functor f => EqF f where
  eqF :: (r -> r -> Bool) -> f r -> f r -> Bool

instance EqF [] where
  eqF eq a b = and (zipWith eq a b) && length a == length b

instance Eq a => EqF (Const a)
  where eqF _ a b = getConst a == getConst b
