{-# LANGUAGE RankNTypes, TypeOperators #-}
module Data.Higher.Foldable where

import Control.Applicative hiding (Const(..))
import Data.Functor.Const
import Data.Higher.Transformation
import Data.Monoid

class HFoldable f where
  hfoldMap :: (Monad c, Alternative m) => (c ~> m) -> f c ~> m

  hfold :: (Monad m, Alternative m) => f m ~> m
  hfold = hfoldMap id

  hlength :: Monad c => f c a -> Int
  hlength = getSum . getConst . hfoldMap (const (Const (Sum 1)))
