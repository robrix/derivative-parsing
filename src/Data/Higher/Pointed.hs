{-# LANGUAGE PolyKinds, TypeOperators #-}
module Data.Higher.Pointed where

import Data.Higher.Transformation

class HPointed p where
  hpoint :: v ~> p v
