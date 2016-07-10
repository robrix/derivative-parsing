{-# LANGUAGE PolyKinds, RankNTypes, TypeOperators #-}
module Data.Higher.Bifunctor where

import Data.Higher.Transformation

class HBifunctor p where
  hbimap :: (a ~> c) -> (b ~> d) -> p a b ~> p c d

  hfirst :: (a ~> c) -> p a b ~> p c b
  hfirst = (`hbimap` id)

  hsecond :: (b ~> d) -> p a b ~> p a d
  hsecond = hbimap id