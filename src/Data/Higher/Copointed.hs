{-# LANGUAGE PolyKinds, TypeOperators #-}
module Data.Higher.Copointed where

import Data.Higher.Transformation

class HCopointed c where
  hcopoint :: c v ~> v
