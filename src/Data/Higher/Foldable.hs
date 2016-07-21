{-# LANGUAGE MultiParamTypeClasses, RankNTypes, TypeOperators #-}
module Data.Higher.Foldable where

import Data.Higher.Monoid
import Data.Higher.Transformation

class HFoldable f a where
  hfoldMap :: HMonoid m => (a ~> m) -> f a ~> m

  hfold :: HMonoid a => f a ~> a
  hfold = hfoldMap id
