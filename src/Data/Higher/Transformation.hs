{-# LANGUAGE RankNTypes, TypeOperators #-}
module Data.Higher.Transformation where

infixr 9 ~>

type f ~> g = (forall a. f a -> g a)
