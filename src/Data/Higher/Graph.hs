{-# LANGUAGE FlexibleInstances, InstanceSigs, RankNTypes, ScopedTypeVariables, TypeOperators #-}
module Data.Higher.Graph
( Rec(..)
, Graph(..)
, gfold
, grfold
, fold
, rfold
, cfold
, sfold
, transform
, graphMap
, liftRec
, pjoin
, modifyGraph
, unroll
, unrollGraph
) where

import Control.Applicative
import Data.Bifunctor (first)
import Data.Function
import Data.Higher.Eq
import Data.Higher.Functor
import Data.Higher.Functor.Eq
import Data.Higher.Functor.Show
import Data.Higher.Transformation

data Rec f v a
  = Var (v a)
  | Mu (v a -> f (Rec f v) a)
  | In (f (Rec f v) a)

newtype Graph f a = Graph { unGraph :: forall v. Rec f v a }


-- Folds

gfold :: HFunctor f => (v ~> c) -> (forall a. (v a -> c a) -> c a) -> (f c ~> c) -> Graph f ~> c
gfold var bind recur = grfold var bind recur . unGraph

grfold :: HFunctor f => (v ~> c) -> (forall a. (v a -> c a) -> c a) -> (f c ~> c) -> Rec f v ~> c
grfold var bind algebra rec = case rec of
  Var x -> var x
  Mu g -> bind (algebra . hfmap recur . g)
  In fa -> algebra (hfmap recur fa)
  where recur = grfold var bind algebra

fold :: HFunctor f => (f c ~> c) -> (forall a. c a) -> Graph f ~> c
fold alg k = rfold alg k . unGraph

rfold :: HFunctor f => (f c ~> c) -> (forall a. c a) -> Rec f c ~> c
rfold alg k = grfold id ($ k) alg

cfold :: HFunctor f => (f t ~> t) -> Graph f ~> t
cfold = gfold id fix

sfold :: (HFunctor f, HEq c) => (f c ~> c) -> (forall a. c a) -> Graph f ~> c
sfold alg k = gfold id (fixVal k) alg


-- Maps

transform :: HFunctor f => (forall v. f (Rec g v) ~> g (Rec g v)) -> Graph f ~> Graph g
transform f = modifyGraph (graphMap f)

graphMap :: HFunctor f => (f (Rec g v) ~> g (Rec g v)) -> Rec f v ~> Rec g v
graphMap f rec = case rec of
  Var x -> Var x
  Mu g -> Mu (f . hfmap (graphMap f) . g)
  In x -> In (f (hfmap (graphMap f) x))

liftRec :: (f (Rec f v) ~> f (Rec f v)) -> Rec f v ~> Rec f v
liftRec f rec = case rec of
  Var v -> Var v
  Mu g -> Mu (f . g)
  In r -> In (f r)

pjoin :: HFunctor f => Rec f (Rec f v) ~> Rec f v
pjoin rec = case rec of
  Var x -> x
  Mu g -> Mu (hfmap pjoin . g . Var)
  In r -> In (hfmap pjoin r)

preturn :: v ~> Rec f v
preturn = Var

modifyGraph :: (forall v. Rec f v ~> Rec g v) -> Graph f ~> Graph g
modifyGraph f g = Graph (f (unGraph g))

unroll :: HFunctor f => Rec f (Rec f v) a -> Rec f (Rec f v) a
unroll rec = case rec of
  Var v -> Var v
  Mu g -> In (g (pjoin (unroll (Mu g))))
  In r -> In (hfmap unroll r)

unrollGraph :: HFunctor f => Graph f ~> Graph f
unrollGraph g = Graph (pjoin (unroll (unGraph g)))


-- Equality

eqRec :: HEqF f => Int -> Rec f (Const Int) a -> Rec f (Const Int) a -> Bool
eqRec n a b = case (a, b) of
  (Var x, Var y) -> x == y
  (Mu g, Mu h) -> let a = g (Const (succ n))
                      b = h (Const (succ n)) in
                      heqF (eqRec (succ n)) a b
  (In x, In y) -> heqF (eqRec n) x y
  _ -> False


-- Show

showsRec :: HShowF f => (forall b. [Const Char b]) -> Int -> Rec f (Const Char) a -> ShowS
showsRec s n rec = case rec of
  Var c -> showChar (getConst c)
  Mu g -> let (a, s') = (head s, tail s) in
              showString "Mu (\\ " . showChar (getConst a) . showString " ->\n  "
              . hshowsPrecF n (showsRec s') (g a) . showString "\n)\n"
  In fa -> hshowsPrecF n (showsRec s) fa


-- Implementation details

fixVal :: HEq h => h a -> (h a -> h a) -> h a
fixVal v f = if v `heq` v' then v else fixVal v' f
  where v' = f v


-- Instances

instance HEqF f => Eq (Graph f a)
  where a == b = eqRec 0 (unGraph a) (unGraph b)

instance HShowF f => Show (Graph f a)
  where showsPrec n = showsRec (iterate (first succ) (Const 'a')) n . unGraph

instance HShowF f => Show (Rec f (Const Char) a)
  where showsPrec = showsRec (iterate (first succ) (Const 'a'))
