{-# LANGUAGE MultiParamTypeClasses, RankNTypes, TypeOperators #-}
module Data.Higher.Foldable where

import Control.Applicative
import Data.Higher.Transformation

class HFoldable f a where
  hfoldMap :: Alternative m => (a ~> m) -> f a ~> m
