{-# LANGUAGE PolyKinds, TypeFamilies #-}
module Control.Higher.Comonad.Cofree where

import Data.Higher.Functor.Foldable

data CofreeF f v b a = v a :< f b a

newtype Cofree f v a = Cofree { runCofree :: CofreeF f v (Cofree f v) a }


-- Instances

type instance Base (Cofree f v) = CofreeF f v
