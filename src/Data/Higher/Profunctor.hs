{-# LANGUAGE RankNTypes, TypeOperators #-}
module Data.Higher.Profunctor where

import Data.Higher.Transformation

class HProfunctor p where
  hdimap :: (a ~> b) -> (c ~> d) -> p b c ~> p a d

  hlmap :: (a ~> b) -> p b c ~> p a c
  hlmap = (`hdimap` id)

  hrmap :: (c ~> d) -> p b c ~> p b d
  hrmap = hdimap id
