{-# LANGUAGE RankNTypes, TypeOperators #-}
module Data.Higher.Product where

import Data.Higher.Functor
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


hfirst :: (f ~> f') -> (f :*: g) ~> (f' :*: g)
hfirst = (*** id)

hsecond :: (g ~> g') -> (f :*: g) ~> (f :*: g')
hsecond = (id ***)

infixr `hdistribute`

hdistribute :: HFunctor f => (f c ~> c') -> (f d ~> d') -> f (c :*: d) ~> (c' :*: d')
hdistribute f g p = f (hfmap hfst p) :*: g (hfmap hsnd p)
