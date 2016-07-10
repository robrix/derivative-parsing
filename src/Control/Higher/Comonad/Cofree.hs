{-# LANGUAGE PolyKinds, TypeFamilies #-}
module Control.Higher.Comonad.Cofree where

import Data.Higher.Bifunctor
import Data.Higher.Functor
import Data.Higher.Functor.Foldable

data CofreeF f v b a = v a :< f b a

newtype Cofree f v a = Cofree { runCofree :: CofreeF f v (Cofree f v) a }

cofree :: CofreeF f v (Cofree f v) a -> Cofree f v a
cofree = Cofree


-- Instances

instance HFunctor f => HBifunctor (CofreeF f) where
  hbimap f g (va :< fba) = f va :< hfmap g fba


type instance Base (Cofree f v) = CofreeF f v
