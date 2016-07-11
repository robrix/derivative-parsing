{-# LANGUAGE PolyKinds, RankNTypes, TypeOperators #-}
module Data.Higher.Functor where

import Data.Higher.Transformation

class HFunctor f where
  hfmap :: (a ~> b) -> f a ~> f b
