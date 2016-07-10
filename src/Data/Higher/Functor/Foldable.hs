{-# LANGUAGE FlexibleContexts, PolyKinds, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Data.Higher.Functor.Foldable where

import Control.Arrow ((&&&))
import Control.Higher.Comonad
import Control.Higher.Comonad.Cofree
import Control.Higher.Monad.Free
import Data.Higher.Bifunctor
import Data.Higher.Functor
import Data.Higher.Functor.Identity
import Data.Higher.Product
import Data.Higher.Profunctor
import Data.Higher.Sum
import Data.Higher.Transformation

type family Base (t :: k -> *) :: (k -> *) -> k -> *

class HFunctor (Base t) => Recursive t where
  project :: t ~> Base t t

  cata :: forall c. (Base t c ~> c) -> t ~> c
  cata f = go
    where go :: t ~> c
          go = f . hfmap go . project

  para :: forall c. (Base t (t :*: c) ~> c) -> t ~> c
  para f = go
    where go :: t ~> c
          go = f . hfmap ((:*:) <*> go) . project

class HFunctor (Base t) => Corecursive t where
  embed :: Base t t ~> t

  ana :: forall c. (c ~> Base t c) -> c ~> t
  ana f = go
    where go :: c ~> t
          go = embed . hfmap go . f

  apo :: forall c. (c ~> Base t (t :+: c)) -> c ~> t
  apo f = go
    where go :: c ~> t
          go = embed . hfmap (heither id go) . f


hylo :: forall f a b . HFunctor f => (f b ~> b) -> (a ~> f a) -> a ~> b
hylo f g = go
  where go :: a ~> b
        go = f . hfmap go . g


newtype Fix f a = Fix { unFix :: f (Fix f) a }


agcata :: forall t w a. (Recursive t, HComonad w) => (forall b. Base t (w b) ~> w (Base t b)) -> (Base t (w a) ~> a) -> t ~> w a
agcata dist alg = go
  where go :: t ~> w a
        go = hfmap alg . dist . hfmap (hduplicate . go) . project

histo :: Recursive t => (Base t (Cofree (Base t) a) ~> a) -> t ~> a
histo alg = extract . agcata distHisto alg

ahisto :: Recursive t => (Base t (Cofree (Base t) a) ~> a) -> t ~> Cofree (Base t) a
ahisto = agcata distHisto

distCata :: HFunctor f => f (Identity a) ~> Identity (f a)
distCata = Identity . hfmap runIdentity

distHisto :: HFunctor f => f (Cofree f a) ~> Cofree f (f a)
distHisto = distGHisto id

distGHisto :: (HFunctor f, HFunctor h) => (forall b. f (h b) ~> h (f b)) -> f (Cofree h a) ~> Cofree h (f a)
distGHisto k = unfold (\ as -> hextract `hfmap` as :*: k (unwrap `hfmap` as))

acata :: forall c t. (HFunctor (Base t), Recursive t) => (Base t c ~> c) -> t ~> Cofree (Base t) c
acata f = go
  where go :: t ~> Cofree (Base t) c
        go = cofree . uncurry (:<) . (f . hfmap extract &&& id) . hfmap go . project

apara :: forall c t. (HFunctor (Base t), Recursive t) => (Base t (t :*: c) ~> c) -> t ~> Cofree (Base t) c
apara f = go
  where go :: t ~> Cofree (Base t) c
        go = cofree . uncurry (:<) . (f . hfmap (hsecond extract) &&& hfmap hsnd) . hfmap ((:*:) <*> go) . project


unannotate :: (HFunctor (Base t), Corecursive t) => Cofree (Base t) c ~> t
unannotate = cata (embed . tailF)


iter :: forall f v a . HProfunctor f => (f v a ~> a) -> Free (f v) a ~> a
iter f = go
  where go :: Free (f v) a ~> a
        go rec = case runFree rec of
          Pure a -> a
          Impure r -> f (hrmap go r)

aiter :: forall f v a. HProfunctor f => (f v a ~> a) -> Free (f v) a ~> Cofree (FreeF (f v) a) a
aiter f = go
  where go :: Free (f v) a ~> Cofree (FreeF (f v) a) a
        go rec = cofree $ case runFree rec of
          Pure a -> a :< Pure a
          Impure r -> let r' = hrmap go r in f (hrmap extract r') :< Impure r'


-- Instances

type instance Base (Fix f) = f

instance HFunctor f => Recursive (Fix f) where project = unFix


type instance Base (Cofree f v) = CofreeF f v

instance HFunctor f => Recursive (Cofree f v) where project = runCofree
instance HFunctor f => Corecursive (Cofree f v) where embed = cofree


type instance Base (Free f v) = FreeF f v

instance HFunctor f => Recursive (Free f v) where project = runFree
instance HFunctor f => Corecursive (Free f v) where embed = free
