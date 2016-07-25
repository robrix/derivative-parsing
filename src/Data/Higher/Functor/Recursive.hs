{-# LANGUAGE FlexibleContexts, PolyKinds, RankNTypes, TypeOperators #-}
module Data.Higher.Functor.Recursive where

import Data.Higher.Functor
import Data.Higher.Transformation


-- Classes

class HRecursive t where
  hproject :: HFunctor f => t f a ~> f (t f a)

  hcata :: HFunctor f => (f a ~> a) -> t f a ~> a
  hcata f = f . hfmap (hcata f) . hproject

class HCorecursive t where
  hembed :: HFunctor f => f (t f a) ~> t f a

  hana :: HFunctor f => (a ~> f a) -> a ~> t f a
  hana f = hembed . hfmap (hana f) . f
