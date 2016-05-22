{-# LANGUAGE RankNTypes, TypeOperators #-}
module Data.Higher.Bifunctor where

import Data.Higher.Transformation

infixr 3 `hbimap`

class HBifunctor p
  where hbimap :: (f ~> f') -> (g ~> g') -> p f g ~> p f' g'

        hfirst :: (f ~> f') -> p f g ~> p f' g
        hfirst = (`hbimap` id)

        hsecond :: (g ~> g') -> p f g ~> p f g'
        hsecond = (id `hbimap`)
