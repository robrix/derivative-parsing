{-# LANGUAGE PolyKinds, TypeOperators #-}
module Data.Higher.Product where

import Data.Higher.Bifunctor
import Data.Higher.Functor

data (f :*: g) a = (:*:) { hfst :: f a, hsnd :: g a }

huncurry :: (f a -> g a -> h) -> (f :*: g) a -> h
huncurry f (a :*: b) = f a b


-- Instances

instance HBifunctor (:*:) where
  hbimap f g p = f (hfst p) :*: g (hsnd p)

instance HFunctor ((:*:) f) where
  hfmap = hsecond
