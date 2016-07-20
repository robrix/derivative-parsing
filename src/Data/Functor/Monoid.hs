module Data.Functor.Monoid where

class HMonoid a where
  hmempty :: a z

  hmappend :: a z -> a z -> a z
