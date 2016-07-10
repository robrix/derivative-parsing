{-# LANGUAGE InstanceSigs, PolyKinds, RankNTypes, ScopedTypeVariables, TypeOperators #-}
module Control.Higher.Monad.Free where

import Data.Higher.Functor
import Data.Higher.Transformation

data FreeF f v b a
  = Impure (f b a)
  | Pure (v a)

newtype Free f v a = Free { runFree :: FreeF f v (Free f v) a }


wrap :: f (Free f v) a -> Free f v a
wrap = Free . Impure


-- Instances

instance HFunctor f => HFunctor (Free f) where
  hfmap :: forall a b. (a ~> b) -> Free f a ~> Free f b
  hfmap f = go
    where go :: Free f a ~> Free f b
          go r = Free $ case runFree r of
            Pure a -> Pure (f a)
            Impure r -> Impure (hfmap go r)
