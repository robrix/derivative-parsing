module Data.Higher.Monoid where

class HMonoid a where
  hempty :: a z

  happend :: a z -> a z -> a z

instance HMonoid []
  where hempty = []
        as `happend` bs = as `mappend` bs
