{-# LANGUAGE RankNTypes, TypeOperators #-}
module Data.Higher.Foldable where

import Control.Applicative
import Data.Higher.Transformation

class HFoldable f where
  hfoldMap :: (Monad c, Alternative m) => (c ~> m) -> f c ~> m

  hfold :: (Monad m, Alternative m) => f m ~> m
  hfold = hfoldMap id
