module Data.Higher.Monoid where

import Data.Functor.Const

class HMonoid a where
  hempty :: a z

  happend :: a z -> a z -> a z

instance Monoid a => HMonoid (Const a)
  where hempty = Const mempty
        Const a `happend` Const b = Const (a `mappend` b)

instance HMonoid []
  where hempty = []
        as `happend` bs = as `mappend` bs
