{-# LANGUAGE RankNTypes, TypeOperators #-}
module Data.Higher.Bifunctor where

import Data.Higher.Transformation

class HBifunctor h where
  hbimap :: (a ~> b) -> (c ~> d) -> h a c ~> h b d

  hfirst :: (a ~> b) -> h a c ~> h b c
  hfirst f = hbimap f id

  hsecond :: (c ~> d) -> h a c ~> h a d
  hsecond = hbimap id

class HBifunctor1 h where
  hbimap1 :: (a ~> b) -> (c -> d) -> h a c ~> h b d

  hfirst1 :: (a ~> b) -> h a c ~> h b c
  hfirst1 f = hbimap1 f id

  hsecond1 :: (c -> d) -> h a c ~> h a d
  hsecond1 = hbimap1 id
