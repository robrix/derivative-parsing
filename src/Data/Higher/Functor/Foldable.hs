{-# LANGUAGE FlexibleContexts, PolyKinds, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Data.Higher.Functor.Foldable where

import Data.Higher.Functor
import Data.Higher.Transformation

type family Base (t :: j) :: (k -> *) -> k -> *

class HFunctor (Base t) => Recursive t where
  project :: t a -> Base t t a

  cata :: forall c. (Base t c ~> c) -> t ~> c
  cata f = go
    where go :: t ~> c
          go = f . hfmap go . project
