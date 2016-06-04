module Data.Functor.Show where

class Functor f => ShowF f
  where showsPrecF :: Int -> (Int -> r -> ShowS) -> f r -> ShowS
