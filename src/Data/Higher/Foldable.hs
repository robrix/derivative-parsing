{-# LANGUAGE MultiParamTypeClasses, RankNTypes, TypeOperators #-}
module Data.Higher.Foldable where

import Data.Functor.Const
import Data.Higher.Monoid
import Data.Higher.Transformation
import Data.Monoid

class HFoldable f a where
  hfoldMap :: HMonoid m => (a ~> m) -> f a ~> m

  hfold :: HMonoid a => f a ~> a
  hfold = hfoldMap id

  hlength :: f a z -> Int
  hlength = getSum . getConst . hfoldMap (const (Const (Sum 1)))
