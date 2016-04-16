{-# LANGUAGE TypeOperators #-}
module Data.Higher.Product where

newtype a :*: b = Product (a, b)
