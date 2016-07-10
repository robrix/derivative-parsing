{-# LANGUAGE RankNTypes, TypeOperators #-}
module Control.Higher.Comonad where

import Data.Higher.Functor
import Data.Higher.Transformation

class HFunctor w => HComonad w where
  hextract :: w a ~> a

  hduplicate :: w a ~> w (w a)
  hduplicate = hextend id

  hextend :: (w a ~> b) -> w a ~> w b
