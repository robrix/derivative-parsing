{-# LANGUAGE PolyKinds, RankNTypes, TypeOperators #-}
module Control.Higher.Applicative where

import Data.Higher.Functor
import Data.Higher.Pointed
import Data.Higher.Transformation

class HFunctor f => HApply f where
  hfmap2 :: (forall z. a z -> b z -> c z) -> f a z -> f b z -> f c z
  hfmap2 h x y = hfmap (A . h) x <:*:> y

  infixl 4 <:*:>
  (<:*:>) :: f (a ~~> b) z -> f a z -> f b z
  g <:*:> x = hfmap2 unA g x

  infixl 4 *:>
  (*:>) :: f b a -> f c a -> f c a
  a *:> b = const (A id) `hfmap` a <:*:> b

  infixl 4 <:*
  (<:*) :: f c a -> f b a -> f c a
  (<:*) = hfmap2 const

  {-# MINIMAL ((<:*:>) | hfmap2) #-}

class (HApply f, HPointed f) => HApplicative (f :: (k -> *) -> k -> *) where
  hpure :: a ~> f a
  hpure = hpoint
