{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeOperators #-}
module Data.Higher.Function where

import Data.Higher.Transformation

hfix :: forall f g. ((f ~> g) -> f ~> g) -> f ~> g
hfix f = x
  where x :: f ~> g
        x = f x
