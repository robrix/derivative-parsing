{-# LANGUAGE FlexibleContexts, PolyKinds, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Data.Higher.Functor.Foldable where

import Control.Higher.Monad.Free
import Data.Higher.Functor
import Data.Higher.Product
import Data.Higher.Sum
import Data.Higher.Transformation

type family Base (t :: k -> *) :: (k -> *) -> k -> *

class HFunctor (Base t) => Recursive t where
  project :: t ~> Base t t

  cata :: forall c. (Base t c ~> c) -> t ~> c
  cata f = go
    where go :: t ~> c
          go = f . hfmap go . project

  para :: forall c. (Base t (t :*: c) ~> c) -> t ~> c
  para f = go
    where go :: t ~> c
          go = f . hfmap ((:*:) <*> go) . project

class HFunctor (Base t) => Corecursive t where
  embed :: Base t t ~> t

  ana :: forall c. (c ~> Base t c) -> c ~> t
  ana f = go
    where go :: c ~> t
          go = embed . hfmap go . f

  apo :: forall c. (c ~> Base t (t :+: c)) -> c ~> t
  apo f = go
    where go :: c ~> t
          go = embed . hfmap (heither id go) . f


hylo :: forall f a b . HFunctor f => (f b ~> b) -> (a ~> f a) -> a ~> b
hylo f g = go
  where go :: a ~> b
        go = f . hfmap go . g


newtype Fix f a = Fix { unFix :: f (Fix f) a }


iter :: forall f a b. HFunctor f => (a ~> b) -> (f b ~> b) -> Free f a ~> b
iter f alg = go
  where go :: Free f a ~> b
        go rec = case runFree rec of
          Pure a -> f a
          Impure r -> alg (hfmap go r)


-- Instances

type instance Base (Fix f) = f

instance HFunctor f => Recursive (Fix f) where project = unFix
instance HFunctor f => Corecursive (Fix f) where embed = Fix


type instance Base (Free f v) = FreeF f v

instance HFunctor f => Recursive (Free f v) where project = runFree
instance HFunctor f => Corecursive (Free f v) where embed = free
