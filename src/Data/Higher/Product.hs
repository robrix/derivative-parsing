{-# LANGUAGE TypeOperators #-}
module Data.Higher.Product where

import Data.Higher.Bifunctor

data (f :*: g) a = (:*:) { hfst :: f a, hsnd :: g a }


-- Instances

instance HBifunctor (:*:) where
  hbimap f g p = f (hfst p) :*: g (hsnd p)
