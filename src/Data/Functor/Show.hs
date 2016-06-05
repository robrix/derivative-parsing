module Data.Functor.Show where

class ShowF f
  where showsPrecF :: Int -> (Int -> r -> ShowS) -> f r -> ShowS
