{-# LANGUAGE PolyKinds, RankNTypes, TypeOperators #-}
module Control.Higher.Applicative where

import Data.Higher.Functor
import Data.Higher.Transformation

class HFunctor f => HApplicative (f :: (k -> *) -> k -> *) where
  hpure :: a ~> f a

  (<:*:>) :: f (a ~~> b) z -> f a z -> f b z

  (*:>) :: f b a -> f c a -> f c a
  a *:> b = (const (A id) `hfmap` a) <:*:> b

  (<:*) :: f c a -> f b a -> f c a
  (<:*) = hliftA2 const


hliftA2 :: HApplicative f => (forall z. a z -> b z -> c z) -> f a z -> f b z -> f c z
hliftA2 f a b = hfmap (A . f) a <:*:> b
