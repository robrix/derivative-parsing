{-# LANGUAGE PolyKinds, TypeOperators #-}
module Data.Higher.Product where

import Data.Higher.Bifunctor
import Data.Higher.Functor
import Data.Higher.Transformation

infixr 8 :*:
data (f :*: g) a = (:*:) { hfst :: f a, hsnd :: g a }

huncurry :: (f a -> g a -> h) -> (f :*: g) a -> h
huncurry f (a :*: b) = f a b

hcurry :: ((f :*: g) a -> h) -> f a -> g a -> h
hcurry f a b = f (a :*: b)

hswap :: (f :*: g) ~> (g :*: f)
hswap (a :*: b) = b :*: a


-- Instances

instance HBifunctor (:*:) where
  hbimap f g p = f (hfst p) :*: g (hsnd p)

instance HFunctor ((:*:) f) where
  hfmap = hsecond
