{-# LANGUAGE FlexibleContexts, PolyKinds, TypeFamilies #-}
module Data.Higher.Functor.Recursive where

import Data.Higher.Functor
import Data.Kind (Type)

type family Base (t :: k -> Type) :: (k -> Type) -> k -> Type

class HFunctor (Base t) => HCorecursive t where
  hembed :: Base t t a -> t a
