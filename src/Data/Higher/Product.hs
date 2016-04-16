{-# LANGUAGE RankNTypes, TypeOperators #-}
module Data.Higher.Product where

data f :*: g = (forall a. f a) :*: (forall b. g b)
