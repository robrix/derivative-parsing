{-# LANGUAGE FlexibleInstances, InstanceSigs, MultiParamTypeClasses, PolyKinds, RankNTypes, ScopedTypeVariables, TypeOperators #-}
module Data.Higher.Graph
( Rec(..)
, RecF(..)
, Graph(..)
, var
, mu
, wrap
, gfold
, grfold
, fold
, rfold
, transform
, graphMap
, liftRec
, pjoin
, modifyGraph
, eqRec
) where

import Control.Applicative
import Data.Higher.Functor
import Data.Higher.Functor.Eq
import Data.Higher.Functor.Show
import Data.Higher.Transformation

data RecF f v b a
  = Mu (v a -> f b a)
  | In (f b a)

data Rec f v a = Var (v a) | Rec (RecF f v (Rec f v) a)

newtype Graph f a = Graph { unGraph :: forall v. Rec f v a }


-- Smart constructors

var :: v a -> Rec f v a
var = Var

mu :: (v a -> f (Rec f v) a) -> Rec f v a
mu = Rec . Mu

wrap :: f (Rec f v) a -> Rec f v a
wrap = Rec . In


-- Folds

iter :: forall f a b. HFunctor f => (a ~> b) -> (RecF f a b ~> b) -> Rec f a ~> b
iter f alg = go
  where go :: Rec f a ~> b
        go rec = case rec of
          Var a -> f a
          Rec r -> alg (hfmap go r)

gfold :: HFunctor f => (v ~> c) -> (forall a. (v a -> c a) -> c a) -> (f c ~> c) -> Graph f ~> c
gfold var bind recur = grfold var bind recur . unGraph

grfold :: forall f v c. HFunctor f => (v ~> c) -> (forall a. (v a -> c a) -> c a) -> (f c ~> c) -> Rec f v ~> c
grfold var bind algebra = iter var $ \ rec -> case rec of
  Mu g -> bind (algebra . g)
  In fa -> algebra fa

fold :: HFunctor f => (f c ~> c) -> (forall a. c a) -> Graph f ~> c
fold alg k = rfold alg k . unGraph

rfold :: HFunctor f => (f c ~> c) -> (forall a. c a) -> Rec f c ~> c
rfold alg k = grfold id ($ k) alg


-- Maps

transform :: HFunctor f => (forall v. f (Rec g v) ~> g (Rec g v)) -> Graph f ~> Graph g
transform f = modifyGraph (graphMap f)

graphMap :: HFunctor f => (f (Rec g a) ~> g (Rec g a)) -> Rec f a ~> Rec g a
graphMap f = iter var $ \ rc -> case rc of
  Mu g -> mu (f . g)
  In r -> wrap (f r)

liftRec :: (f (Rec f v) ~> g (Rec g v)) -> Rec f v ~> Rec g v
liftRec f rc = case rc of
  Var v -> var v
  Rec (Mu g) -> mu (f . g)
  Rec (In r) -> wrap (f r)

pjoin :: HFunctor f => Rec f (Rec f v) ~> Rec f v
pjoin = iter id $ \ rc -> case rc of
  Mu g -> mu (g . var)
  In r -> wrap r

modifyGraph :: (forall v. Rec f v ~> Rec g v) -> Graph f ~> Graph g
modifyGraph f g = Graph (f (unGraph g))


-- Equality

eqRec :: HEqF f => Int -> Rec f (Const Int) a -> Rec f (Const Int) a -> Bool
eqRec n a b = case (a, b) of
  (Var x, Var y) -> x == y
  (Rec (Mu g), Rec (Mu h)) -> let a = g (Const (succ n))
                                  b = h (Const (succ n)) in
                                  heqF (eqRec (succ n)) a b
  (Rec (In x), Rec (In y)) -> heqF (eqRec n) x y
  _ -> False


-- Show

showsRec :: HShowF f => String -> Int -> Rec f (Const Char) a -> ShowS
showsRec s n rec = case rec of
  Var c -> showChar (getConst c)
  Rec (Mu g) -> showString "Mu (\\ " . showChar (head s) . showString " ->\n  "
                . hshowsPrecF (showsRec (tail s)) n (g (Const (head s))) . showString "\n)\n"
  Rec (In fa) -> hshowsPrecF (showsRec s) n fa


-- Instances

instance HEqF f => Eq (Graph f a)
  where a == b = eqRec 0 (unGraph a) (unGraph b)

instance HEqF f => Eq (Rec f (Const Int) a)
  where a == b = eqRec 0 a b

instance HShowF f => Show (Graph f a)
  where showsPrec n = showsPrec n . (unGraph :: Graph f ~> Rec f (Const Char))

instance HShowF f => Show (Rec f (Const Char) a)
  where showsPrec = showsRec (iterate succ 'a')

instance HFunctor f => HFunctor (RecF f v)
  where hfmap f rec = case rec of
          Mu g -> Mu (hfmap f . g)
          In r -> In (hfmap f r)
