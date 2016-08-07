{-# LANGUAGE PolyKinds, TypeFamilies #-}
module Data.Higher.Functor.Recursive where

type family Base (t :: k -> *) :: (k -> *) -> k -> *
