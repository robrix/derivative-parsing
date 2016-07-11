{-# LANGUAGE InstanceSigs, PolyKinds, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Control.Higher.Monad.Free where

import Data.Higher.Bifunctor
import Data.Higher.Functor
import Data.Higher.Pointed
import Data.Higher.Transformation

data FreeF f v b a
  = Impure (f b a)
  | Pure (v a)

newtype Free f v a = Free { runFree :: FreeF f v (Free f v) a }


free :: FreeF f v (Free f v) ~> Free f v
free = Free

wrap :: f (Free f v) ~> Free f v
wrap = free . Impure


-- Instances

instance HFunctor f => HBifunctor (FreeF f) where
  hbimap f g r = case r of
    Pure a -> Pure (f a)
    Impure r -> Impure (hfmap g r)

instance HFunctor f => HFunctor (FreeF f v) where
  hfmap = hsecond

instance HPointed (Free f) where
  hpoint = free . Pure

instance HFunctor f => HFunctor (Free f) where
  hfmap :: forall a b. (a ~> b) -> Free f a ~> Free f b
  hfmap f = go
    where go :: Free f a ~> Free f b
          go = Free . hbimap f go . runFree
