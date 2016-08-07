{-# LANGUAGE RankNTypes, TypeOperators #-}
module Data.Higher.Functor.Fix where

import Data.Higher.Functor
import Data.Higher.Transformation

newtype Fix f a = Fix { unFix :: f (Fix f) a }

cata :: HFunctor f => (f a ~> a) -> Fix f ~> a
cata algebra = algebra . hfmap (cata algebra) . unFix