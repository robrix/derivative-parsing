{-# LANGUAGE RankNTypes #-}
module Data.Higher.Functor.Show where

import Data.Higher.Functor

class HFunctor f => HShowF f
  where hshowsPrecF :: Int -> (forall a. Int -> r a -> String -> String) -> f r a -> String -> String
