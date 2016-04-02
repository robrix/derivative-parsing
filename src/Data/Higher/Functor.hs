{-# LANGUAGE RankNTypes, TypeOperators #-}
module Data.Higher.Functor where

import Data.Higher.Transformation

class HFunctor h where
  hfmap :: (f ~> g) -> (forall a. h f a -> h g a)
