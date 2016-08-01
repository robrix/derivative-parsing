{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PolyKinds, RankNTypes, TypeOperators #-}
module Data.Higher.Functor.Recursive where

import Data.Higher.Functor
import Data.Higher.Transformation


-- Types

newtype Fix f a = Fix { unFix :: f (Fix f) a }

data Free f v a
  = Pure (v a)
  | Impure (f (Free f v) a)


iter :: HFunctor f => (f a ~> a) -> Free f a ~> a
iter algebra = (\ a -> case a of
  Pure a -> a
  Impure r -> algebra (hfmap (iter algebra) r))


-- Classes

class HFunctor f => HRecursive t f where
  hproject :: t f a ~> f (t f a)

  hcata :: (f a ~> a) -> t f a ~> a
  hcata f = f . hfmap (hcata f) . hproject

class HFunctor f => HCorecursive t f where
  hembed :: f (t f a) ~> t f a

  hana :: (a ~> f a) -> a ~> t f a
  hana f = hembed . hfmap (hana f) . f

class HHoist t where
  hoist :: HFunctor f => (f (t g a) ~> g (t g a)) -> t f a ~> t g a
