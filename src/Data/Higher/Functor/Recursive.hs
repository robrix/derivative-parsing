{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, PolyKinds, RankNTypes, TypeOperators #-}
module Data.Higher.Functor.Recursive where

import Data.Higher.Functor
import Data.Higher.Transformation


-- Classes

class HFunctor f => HRecursive t f where
  hproject :: t f a ~> f (t f a)

  hcata :: (f a ~> a) -> t f a ~> a
  hcata f = f . hfmap (hcata f) . hproject

class HFunctor f => HCorecursive t f where
  hembed :: f (t f a) ~> t f a

  hana :: (a ~> f a) -> a ~> t f a
  hana f = hembed . hfmap (hana f) . f
