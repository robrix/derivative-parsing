{-# LANGUAGE FlexibleContexts, InstanceSigs, RankNTypes, PolyKinds, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Control.Higher.Comonad.Cofree where

import Control.Arrow
import Control.Higher.Comonad
import Data.Higher.Bifunctor
import Data.Higher.Functor
import Data.Higher.Transformation

data CofreeF f v b a = (:<) { headF :: v a, tailF :: f b a }

newtype Cofree f v a = Cofree { runCofree :: CofreeF f v (Cofree f v) a }

cofree :: CofreeF f v (Cofree f v) a -> Cofree f v a
cofree = Cofree

extract :: Cofree f a ~> a
extract = headF . runCofree

unwrap :: Cofree f a ~> f (Cofree f a)
unwrap = tailF . runCofree


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

instance HFunctor f => HComonad (Cofree f) where
  hextract = headF . runCofree

  hduplicate = cofree . uncurry (:<) . (id &&& hfmap hduplicate . unwrap)

  hextend :: forall a b. (Cofree f a ~> b) -> Cofree f a ~> Cofree f b
  hextend f = go
    where go :: Cofree f a ~> Cofree f b
          go = cofree . uncurry (:<) . (f &&& hfmap go . unwrap)
