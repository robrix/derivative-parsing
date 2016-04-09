{-# LANGUAGE RankNTypes, TypeOperators #-}
module Data.Higher.Isofunctor where

import Data.Higher.Transformation

class HIsofunctor f
  where hisomap :: (a ~> b) -> (b ~> a) -> f a ~> f b
