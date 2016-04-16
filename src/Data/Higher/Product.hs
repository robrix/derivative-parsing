{-# LANGUAGE RankNTypes, TypeOperators #-}
module Data.Higher.Product where

data (:*:) f g a = f a :*: g a
