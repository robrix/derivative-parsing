module Data.Functor.Show where

class ShowF f
  where showsPrecF :: Int -> (Int -> a -> ShowS) -> f a -> ShowS
