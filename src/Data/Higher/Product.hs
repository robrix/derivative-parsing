{-# LANGUAGE RankNTypes, TypeOperators #-}
module Data.Higher.Product where

import Data.Higher.Transformation

infixr :*:

data (:*:) f g a = f a :*: g a


infixr 3 ***

(***) :: (f ~> f') -> (g ~> g') -> (f :*: g) ~> (f' :*: g')
f *** g = \ (a :*: b) -> f a :*: g b


hfst :: (f :*: g) ~> f
hfst (f :*: _) = f

hsnd :: (f :*: g) ~> g
hsnd (_ :*: g) = g
