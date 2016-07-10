{-# LANGUAGE PolyKinds, TypeOperators #-}
module Data.Higher.Sum where

data (f :+: g) a
  = L (f a)
  | R (g a)

