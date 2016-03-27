module Data.Higher.Eq where

class HEq f where
  heq :: f a -> f a -> Bool
