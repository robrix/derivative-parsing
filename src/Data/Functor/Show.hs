module Data.Functor.Show where

class Functor f => ShowF f where
  showF :: (r -> String) -> f r -> String
