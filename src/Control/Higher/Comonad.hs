{-# LANGUAGE PolyKinds, RankNTypes, TypeOperators #-}
module Control.Higher.Comonad where

import Data.Higher.Copointed
import Data.Higher.Functor
import Data.Higher.Transformation

class (HFunctor w, HCopointed w) => HComonad w where
  hduplicate :: w a ~> w (w a)
  hduplicate = hextend id

  hextend :: (w a ~> b) -> w a ~> w b
  hextend f = hfmap f . hduplicate

  {-# MINIMAL (hduplicate | hextend) #-}
