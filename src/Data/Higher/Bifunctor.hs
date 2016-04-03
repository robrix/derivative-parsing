{-# LANGUAGE RankNTypes, TypeOperators #-}
module Data.Higher.Bifunctor where

import Data.Higher.Transformation

class HBifunctor h where
  hbimap :: (a ~> b) -> (c ~> d) -> h a c ~> h b d

  hfirst :: (a ~> b) -> h a c ~> h b c
  hfirst f = hbimap f id

  hsecond :: (c ~> d) -> h a c ~> h a d
  hsecond = hbimap id
