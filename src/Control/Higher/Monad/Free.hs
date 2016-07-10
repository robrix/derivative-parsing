{-# LANGUAGE InstanceSigs, PolyKinds, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Control.Higher.Monad.Free where

import Data.Higher.Bifunctor
import Data.Higher.Functor
import Data.Higher.Functor.Foldable
import Data.Higher.Profunctor
import Data.Higher.Transformation

data FreeF f v b a
  = Impure (f b a)
  | Pure (v a)

newtype Free f v a = Free { runFree :: FreeF f v (Free f v) a }


free :: FreeF f v (Free f v) ~> Free f v
free = Free

wrap :: f (Free f v) ~> Free f v
wrap = free . Impure


iter :: forall f v a . HProfunctor f => (f v a ~> a) -> Free (f v) a ~> a
iter f = go
  where go :: Free (f v) a ~> a
        go rec = case runFree rec of
          Pure a -> a
          Impure r -> f (hrmap go r)


-- Instances

instance HFunctor f => HBifunctor (FreeF f) where
  hbimap f g r = case r of
    Pure a -> Pure (f a)
    Impure r -> Impure (hfmap g r)

instance HFunctor f => HFunctor (FreeF f v) where
  hfmap = hsecond

instance HFunctor f => HFunctor (Free f) where
  hfmap :: forall a b. (a ~> b) -> Free f a ~> Free f b
  hfmap f = go
    where go :: Free f a ~> Free f b
          go = Free . hbimap f go . runFree


type instance Base (Free f v) = FreeF f v

instance HFunctor f => Recursive (Free f v) where project = runFree
instance HFunctor f => Corecursive (Free f v) where embed = free
