{-# LANGUAGE PolyKinds, RankNTypes, TypeOperators #-}
module Data.Higher.Functor where

import Data.Higher.Transformation

class HFunctor f where
  infixl 4 `hfmap`
  hfmap :: (a ~> b) -> f a ~> f b
