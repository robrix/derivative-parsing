{-# LANGUAGE TypeOperators #-}
module Data.Higher.Product where

data a :*: b = a :*: b
  deriving (Eq, Show)
