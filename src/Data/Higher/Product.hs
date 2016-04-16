{-# LANGUAGE RankNTypes, TypeOperators #-}
module Data.Higher.Product where

infixr :*:

data (:*:) f g a = f a :*: g a
