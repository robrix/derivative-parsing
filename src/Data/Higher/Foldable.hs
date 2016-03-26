{-# LANGUAGE RankNTypes #-}
module Data.Higher.Foldable where

class HFoldable h where
  hfoldMap :: Monoid m => (forall b. f b -> m) -> h f a -> m
