{-# LANGUAGE RankNTypes, TypeOperators #-}
module Data.Higher.Transformation where

type f ~> g = (forall a. f a -> g a)
