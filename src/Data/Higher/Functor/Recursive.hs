{-# LANGUAGE FlexibleContexts, PolyKinds, TypeFamilies #-}
module Data.Higher.Functor.Recursive where

import Data.Higher.Functor

type family Base (t :: k -> *) :: (k -> *) -> k -> *

class HFunctor (Base t) => HCorecursive t where
  hembed :: Base t t a -> t a
