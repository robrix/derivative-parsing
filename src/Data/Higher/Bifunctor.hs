{-# LANGUAGE RankNTypes, TypeOperators #-}
module Data.Higher.Bifunctor where

import Data.Higher.Transformation

class HBifunctor p
  where hbimap :: (f ~> f') -> (g ~> g') -> p f g ~> p f' g'
