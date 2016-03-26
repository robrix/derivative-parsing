{-# LANGUAGE RankNTypes #-}
module Data.Higher.Functor where

class HFunctor h where
  hfmap :: (forall a. f a -> g a) -> (forall a. h f a -> h g a)
