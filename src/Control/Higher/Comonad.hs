{-# LANGUAGE RankNTypes, TypeOperators #-}
module Control.Higher.Comonad where

import Data.Higher.Transformation

class HComonad w where
  hextract :: w a ~> a

  hextend :: (w a ~> b) -> w a ~> w b
