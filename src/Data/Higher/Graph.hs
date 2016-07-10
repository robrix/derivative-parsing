{-# LANGUAGE FlexibleInstances, InstanceSigs, PolyKinds, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Data.Higher.Graph
( Rec(..)
, RecF(..)
, Graph(..)
, var
, mu
, rec
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

import Data.Bifunctor (first)
import Data.Function
import Data.Higher.Eq
import Data.Higher.Foldable
import Data.Higher.Functor
import Data.Higher.Functor.Eq
import Data.Higher.Functor.Foldable
import Data.Higher.Functor.Show
import Data.Higher.Transformation

data RecF f v b a
  = Var (v a)
  | Mu (v a -> f b a)
  | In (f b a)

newtype Rec f v a = Rec { unRec :: RecF f v (Rec f v) a }

newtype Graph f a = Graph { unGraph :: forall v. Rec f v a }


-- Smart constructors

var :: v a -> Rec f v a
var = Rec . Var

mu :: (v a -> f (Rec f v) a) -> Rec f v a
mu = Rec . Mu

rec :: f (Rec f v) a -> Rec f v a
rec = Rec . In


-- Folds

gfold :: HFunctor f => (v ~> c) -> (forall a. (v a -> c a) -> c a) -> (f c ~> c) -> Graph f ~> c
gfold var bind recur = grfold var bind recur . unGraph

grfold :: forall f v c. HFunctor f => (v ~> c) -> (forall a. (v a -> c a) -> c a) -> (f c ~> c) -> Rec f v ~> c
grfold var bind algebra = cata $ \ rec -> case rec of
  Var x -> var x
  Mu g -> bind (algebra . g)
  In fa -> algebra fa

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
graphMap f = cata $ \ rec -> Rec $ case rec of
  Var x -> Var x
  Mu g -> Mu (f . g)
  In x -> In (f x)

liftRec :: (f (Rec f v) ~> f (Rec f v)) -> Rec f v ~> Rec f v
liftRec f rec = Rec $ case unRec rec of
  Var v -> Var v
  Mu g -> Mu (f . g)
  In r -> In (f r)

pjoin :: HFunctor f => Rec f (Rec f v) ~> Rec f v
pjoin = cata $ \ rc -> case rc of
  Var x -> x
  Mu g -> mu (g . var)
  In r -> rec r

preturn :: v ~> Rec f v
preturn = Rec . Var

modifyGraph :: (forall v. Rec f v ~> Rec g v) -> Graph f ~> Graph g
modifyGraph f g = Graph (f (unGraph g))

unroll :: HFunctor f => Rec f (Rec f v) a -> Rec f (Rec f v) a
unroll rec = Rec $ case unRec rec of
  Var v -> Var v
  Mu g -> In (g (pjoin (unroll (Rec (Mu g)))))
  In r -> In (hfmap unroll r)

unrollGraph :: HFunctor f => Graph f ~> Graph f
unrollGraph g = Graph (pjoin (unroll (unGraph g)))


-- Equality

eqRec :: HEqF f => Int -> Rec f (Const Int) a -> Rec f (Const Int) a -> Bool
eqRec n a b = case (unRec a, unRec b) of
  (Var x, Var y) -> x == y
  (Mu g, Mu h) -> let a = g (Const (succ n))
                      b = h (Const (succ n)) in
                      heqF (eqRec (succ n)) a b
  (In x, In y) -> heqF (eqRec n) x y
  _ -> False


-- Show

showsRec :: HShowF f => (forall b. [Const Char b]) -> Int -> Rec f (Const Char) a -> ShowS
showsRec s n rec = case unRec rec of
  Var c -> showChar (getConst c)
  Mu g -> let (a, s') = (head s, tail s) in
              showString "Mu (\\ " . showChar (getConst a) . showString " ->\n  "
              . hshowsPrecF n (showsRec (fmap (Const . getConst) s')) (g a) . showString "\n)\n"
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

instance HFunctor f => HFunctor (RecF f v)
  where hfmap f rec = case rec of
          Var v -> Var v
          Mu g -> Mu (hfmap f . g)
          In r -> In (hfmap f r)

type instance Base (Rec f v) = RecF f v

instance HFunctor f => Recursive (Rec f v) where project = unRec

class HIsofunctor f
  where hisomap :: (a ~> b) -> (b ~> a) -> (f a z -> f b z, f b z -> f a z)

instance HFunctor f => HIsofunctor (Rec f)
  where hisomap :: forall a b z. (a ~> b) -> (b ~> a) -> (Rec f a z -> Rec f b z, Rec f b z -> Rec f a z)
        hisomap f g = (to, from)
          where to :: Rec f a ~> Rec f b
                to rec = Rec $ case unRec rec of
                  Var v -> Var (f v)
                  Mu h -> Mu (hfmap to . h . g)
                  In r -> In (hfmap to r)
                from :: Rec f b ~> Rec f a
                from rec = Rec $ case unRec rec of
                  Var v -> Var (g v)
                  Mu h -> Mu (hfmap from . h . f)
                  In r -> In (hfmap from r)

instance HFunctor f => HIsofunctor (RecF f v) where
  hisomap f g = (hfmap f, hfmap g)
