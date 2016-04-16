{-# LANGUAGE GeneralizedNewtypeDeriving, TypeOperators #-}
module Data.Higher.Product where

newtype a :*: b = Product (a, b)
  deriving (Eq, Show)
