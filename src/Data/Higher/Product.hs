{-# LANGUAGE TypeOperators #-}
module Data.Higher.Product where

data (f :*: g) a = (:*:) { hfst :: f a, hsnd :: g a }
