{-# LANGUAGE MultiParamTypeClasses, RankNTypes, TypeOperators #-}
module Data.Higher.Foldable where

import Data.Functor.Const
import Data.Higher.Monoid
import Data.Higher.Transformation
import Data.Monoid

class HFoldable f a where
  hfoldMap :: HMonoid m => (a ~> m) -> f a ~> m

  hfoldr :: (forall z. a z -> b z -> b z) -> forall z. b z -> f a z -> b z
  hfoldr f z t = appHEndo (hfoldMap (HEndo . f) t) z

  hfold :: HMonoid a => f a ~> a
  hfold = hfoldMap id

  hlength :: f a z -> Int
  hlength = getSum . getConst . hfoldMap (const (Const (Sum 1)))

newtype HEndo a z = HEndo { appHEndo :: a z -> a z }

instance HMonoid (HEndo a) where
  hempty = HEndo id
  HEndo a `happend` HEndo b = HEndo (a . b)
