{-# LANGUAGE RankNTypes #-}
module Data.Higher.Traversable where

import Data.Higher.Functor

class HFunctor t => HTraversable t where
  htraverse :: Applicative f => (forall z. a z -> f (b z)) -> t a z -> f (t b z)
