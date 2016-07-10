{-# LANGUAGE PolyKinds, RankNTypes, TypeOperators #-}
module Control.Higher.Applicative where

import Data.Higher.Functor
import Data.Higher.Transformation

class HFunctor f => HApplicative (f :: (k -> *) -> k -> *) where
  hpure :: a ~> f a
