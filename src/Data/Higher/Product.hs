{-# LANGUAGE RankNTypes, TypeOperators #-}
module Data.Higher.Product where

import Data.Higher.Functor
import Data.Higher.Transformation

infixr :*:

data (:*:) f g a = f a :*: g a


infixr 3 ***

(***) :: (f ~> f') -> (g ~> g') -> (f :*: g) ~> (f' :*: g')
f *** g = \ (a :*: b) -> f a :*: g b


-- | Retrieve the first field of a higher product.
hfst :: (f :*: g) ~> f
hfst (f :*: _) = f

-- | Retrieve the second field of a higher product.
hsnd :: (f :*: g) ~> g
hsnd (_ :*: g) = g


-- | Map over the first field of a higher product, leaving the second field unchanged.
hfirst :: (f ~> f') -> (f :*: g) ~> (f' :*: g)
hfirst = (*** id)

-- | Map over the second field of a higher product, leaving the first field unchanged.
hsecond :: (g ~> g') -> (f :*: g) ~> (f :*: g')
hsecond = (id ***)

infixr `hdistribute`

hdistribute :: HFunctor f => (f c ~> c') -> (f d ~> d') -> f (c :*: d) ~> (c' :*: d')
hdistribute f g p = f (hfmap hfst p) :*: g (hfmap hsnd p)
