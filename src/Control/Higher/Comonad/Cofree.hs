{-# LANGUAGE FlexibleContexts, InstanceSigs, RankNTypes, PolyKinds, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Control.Higher.Comonad.Cofree where

import Control.Arrow
import Data.Higher.Bifunctor
import Data.Higher.Functor
import Data.Higher.Functor.Foldable
import Data.Higher.Transformation

data CofreeF f v b a = (:<) { headF :: v a, tailF :: f b a }

newtype Cofree f v a = Cofree { runCofree :: CofreeF f v (Cofree f v) a }

cofree :: CofreeF f v (Cofree f v) a -> Cofree f v a
cofree = Cofree

extract :: Cofree f a ~> a
extract = headF . runCofree


acata :: forall c t. (HFunctor (Base t), Recursive t) => (Base t c ~> c) -> t ~> Cofree (Base t) c
acata f = go
  where go :: t ~> Cofree (Base t) c
        go = cofree . uncurry (:<) . (f . hfmap extract &&& id) . hfmap go . project


-- Instances

instance HFunctor f => HBifunctor (CofreeF f) where
  hbimap f g (va :< fba) = f va :< hfmap g fba

instance HFunctor f => HFunctor (CofreeF f v) where
  hfmap = hsecond

instance HFunctor f => HFunctor (Cofree f) where
  hfmap :: forall a b. (a ~> b) -> Cofree f a ~> Cofree f b
  hfmap f = go
    where go :: Cofree f a ~> Cofree f b
          go = cofree . hbimap f go . runCofree


type instance Base (Cofree f v) = CofreeF f v

instance HFunctor f => Recursive (Cofree f v) where project = runCofree
