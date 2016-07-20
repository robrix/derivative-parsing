module Data.Higher.Monoid where

class HMonoid a where
  hempty :: a z

  happend :: a z -> a z -> a z
