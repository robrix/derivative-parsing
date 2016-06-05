{-# LANGUAGE RankNTypes #-}
module Data.Higher.Functor.Show where

class HShowF f
  where hshowsPrecF :: Int -> (forall a. Int -> r a -> ShowS) -> f r a -> ShowS
