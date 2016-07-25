{-# LANGUAGE FlexibleContexts, PolyKinds, TypeFamilies #-}
module Data.Higher.Functor.Recursive where

import Data.Higher.Functor

type family Base (t :: k -> *) :: (k -> *) -> k -> *

class HFunctor (Base t) => HRecursive t where
  hproject :: t a -> Base t t a
