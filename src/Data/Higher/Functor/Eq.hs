{-# LANGUAGE RankNTypes #-}
module Data.Higher.Functor.Eq where

import Data.Higher.Functor

class HFunctor f => HEqF f
  where heqF :: (forall a. r a -> r a -> Bool) -> f r a -> f r a -> Bool
