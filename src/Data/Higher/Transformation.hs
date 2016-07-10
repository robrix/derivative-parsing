{-# LANGUAGE PolyKinds, RankNTypes, TypeOperators #-}
module Data.Higher.Transformation where

infixr 9 ~>
type f ~> g = (forall a. f a -> g a)

infixr 9 ~~>
newtype (f ~~> g) a = A { unA :: f a -> g a }
