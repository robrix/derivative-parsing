{-# LANGUAGE PolyKinds, RankNTypes, TypeOperators #-}
module Data.Higher.Functor where

import Data.Higher.Transformation

class HFunctor f where
  infixl 4 `hfmap`
  hfmap :: (a ~> b) -> f a ~> f b

  (<:$) :: (forall z. a z) -> f b ~> f a
  (<:$) z = hfmap (const z)

infixl 4 <:$:>
(<:$:>) :: HFunctor f => (a ~> b) -> f a ~> f b
(<:$:>) = hfmap
