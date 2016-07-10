{-# LANGUAGE RankNTypes #-}
module Data.Higher.Foldable where

import Control.Applicative

class HFoldable f where
  hfoldMap :: Monoid m => (forall a. c a -> m) -> f c a -> m

  hfold :: Monoid m => f (Const m) a -> m
  hfold = hfoldMap getConst
