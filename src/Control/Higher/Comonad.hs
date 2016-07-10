{-# LANGUAGE TypeOperators #-}
module Control.Higher.Comonad where

import Data.Higher.Transformation

class HComonad w where
  hextract :: w a ~> a
