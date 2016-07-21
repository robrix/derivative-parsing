{-# LANGUAGE InstanceSigs, PolyKinds, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Control.Higher.Monad.Free where

import Data.Higher.Functor
import Data.Higher.Transformation

data FreeF f v b a
  = Impure (f b a)
  | Pure (v a)

newtype Free f v a = Free { runFree :: FreeF f v (Free f v) a }


free :: FreeF f v (Free f v) ~> Free f v
free = Free

wrap :: f (Free f v) ~> Free f v
wrap = free . Impure

unFree :: Free f v a -> Either (v a) (f (Free f v) a)
unFree r = case runFree r of
  Pure a -> Left a
  Impure f -> Right f

iter :: forall f a b. HFunctor f => (a ~> b) -> (f b ~> b) -> Free f a ~> b
iter f alg = go
  where go :: Free f a ~> b
        go rec = case runFree rec of
          Pure a -> f a
          Impure r -> alg (hfmap go r)

hbimap :: HFunctor f => (a ~> c) -> (b ~> d) -> FreeF f a b ~> FreeF f c d
hbimap f g r = case r of
  Pure a -> Pure (f a)
  Impure r -> Impure (hfmap g r)

-- Instances

instance HFunctor f => HFunctor (FreeF f v) where
  hfmap = hbimap id

instance HFunctor f => HFunctor (Free f) where
  hfmap :: forall a b. (a ~> b) -> Free f a ~> Free f b
  hfmap f = go
    where go :: Free f a ~> Free f b
          go = Free . hbimap f go . runFree
