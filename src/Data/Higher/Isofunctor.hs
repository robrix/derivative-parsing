{-# LANGUAGE RankNTypes, TypeOperators #-}
module Data.Higher.Isofunctor where

import Data.Higher.Transformation

class HIsofunctor f
  where hisomap :: (c ~> d) -> (d ~> c) -> (f c a -> f d a, f d a -> f c a)
