{-# LANGUAGE RankNTypes #-}
module Data.Higher.Functor.Show where

class HShowF f
  where hshowsPrecF :: (forall a. Int -> r a -> ShowS) -> Int -> f r a -> ShowS
