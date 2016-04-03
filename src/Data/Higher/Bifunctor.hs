{-# LANGUAGE RankNTypes, TypeOperators #-}
module Data.Higher.Bifunctor where

import Data.Higher.Transformation

class HBifunctor h where
  hbimap :: (a ~> b) -> (c ~> d) -> h a c ~> h b d
