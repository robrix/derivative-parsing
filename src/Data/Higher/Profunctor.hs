{-# LANGUAGE RankNTypes, TypeOperators #-}
module Data.Higher.Profunctor where

import Data.Higher.Transformation

class HProfunctor p where
  hdimap :: (a ~> b) -> (c ~> d) -> p b c ~> p a d
