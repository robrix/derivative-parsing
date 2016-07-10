{-# LANGUAGE PolyKinds #-}
module Control.Higher.Comonad.Cofree where

data CofreeF f v b a = v a :< f b a

newtype Cofree f v a = Cofree { runCofree :: CofreeF f v (Cofree f v) a }
