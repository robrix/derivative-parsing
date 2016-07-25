{-# LANGUAGE FlexibleContexts, PolyKinds, RankNTypes, TypeFamilies, TypeOperators #-}
module Data.Higher.Functor.Recursive where

import Data.Higher.Functor
import Data.Higher.Transformation

type family Base (t :: k -> *) :: (k -> *) -> k -> *


-- Classes

class HFunctor (Base t) => HRecursive t where
  hproject :: t ~> Base t t

  hcata :: (Base t a ~> a) -> t ~> a
  hcata f = f . hfmap (hcata f) . hproject

class HFunctor (Base t) => HCorecursive t where
  hembed :: Base t t ~> t

  hana :: (a ~> Base t a) -> a ~> t
  hana f = hembed . hfmap (hana f) . f
