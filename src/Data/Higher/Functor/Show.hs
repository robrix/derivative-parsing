{-# LANGUAGE RankNTypes #-}
module Data.Higher.Functor.Show where

import Data.Higher.Functor

class HFunctor f => HShowF f
  where hshowF :: (forall a. r a -> String) -> f r a -> String
