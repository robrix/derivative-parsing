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
, agfold
, agrfold
, fold
, rfold
, afold
, arfold
, cfold
, sfold
, transform
, graphMap
, agraphMap
, liftRec
, pjoin
, modifyGraph
, unroll
, unrollGraph
) where

import Control.Higher.Comonad.Cofree
import Control.Higher.Monad.Free
import Data.Bifunctor (first)
import Data.Function
import Data.Higher.Eq
import Data.Higher.Foldable
import Data.Higher.Functor
import Data.Higher.Functor.Eq
import Data.Higher.Functor.Foldable
import Data.Higher.Functor.Show
import Data.Higher.Profunctor
import Data.Higher.Transformation

data RecF f v b a
  = Mu (v a -> f b a)
  | In (f b a)

newtype Rec f v a = Rec { unRec :: FreeF (RecF f v) v (Rec f v) a }

newtype Graph f a = Graph { unGraph :: forall v. Rec f v a }


-- Smart constructors

var :: v a -> Rec f v a
var = Rec . Pure

mu :: (v a -> f (Rec f v) a) -> Rec f v a
mu = Rec . Impure . Mu

rec :: f (Rec f v) a -> Rec f v a
rec = Rec . Impure . In


toFree :: HFunctor f => Rec f a ~> Free (RecF f a) a
toFree = hylo free unRec

fromFree :: HFunctor f => Free (RecF f a) a ~> Rec f a
fromFree = hylo Rec runFree


-- Folds

gfold :: HFunctor f => (v ~> c) -> (forall a. (v a -> c a) -> c a) -> (f c ~> c) -> Graph f ~> c
gfold var bind recur = grfold var bind recur . unGraph

grfold :: forall f v c. HFunctor f => (v ~> c) -> (forall a. (v a -> c a) -> c a) -> (f c ~> c) -> Rec f v ~> c
grfold var bind algebra = cata $ \ rec -> case rec of
  Pure x -> var x
  Impure (Mu g) -> bind (algebra . g)
  Impure (In fa) -> algebra fa

agfold :: HFunctor f => (v ~> c) -> (forall a. (v a -> c a) -> c a) -> (f c ~> c) -> Graph f ~> Cofree (FreeF (RecF f v) v) c
agfold var bind recur = agrfold var bind recur . unGraph

agrfold :: HFunctor f => (v ~> c) -> (forall a. (v a -> c a) -> c a) -> (f c ~> c) -> Rec f v ~> Cofree (FreeF (RecF f v) v) c
agrfold var bind algebra = acata $ \ rec -> case rec of
  Pure x -> var x
  Impure (Mu g) -> bind (algebra . g)
  Impure (In fa) -> algebra fa

fold :: HFunctor f => (f c ~> c) -> (forall a. c a) -> Graph f ~> c
fold alg k = rfold alg k . unGraph

rfold :: HFunctor f => (f c ~> c) -> (forall a. c a) -> Rec f c ~> c
rfold alg k = grfold id ($ k) alg

afold :: HFunctor f => (f c ~> c) -> (forall a. c a) -> Graph f ~> Cofree (FreeF (RecF f c) c) c
afold alg k = arfold alg k . unGraph

arfold :: HFunctor f => (f c ~> c) -> (forall a. c a) -> Rec f c ~> Cofree (FreeF (RecF f c) c) c
arfold alg k = agrfold id ($ k) alg

cfold :: HFunctor f => (f t ~> t) -> Graph f ~> t
cfold = gfold id fix

sfold :: (HFunctor f, HEq c) => (f c ~> c) -> (forall a. c a) -> Graph f ~> c
sfold alg k = gfold id (fixVal k) alg


-- Maps

transform :: HFunctor f => (forall v. f (Rec g v) ~> g (Rec g v)) -> Graph f ~> Graph g
transform f = modifyGraph (graphMap f)

graphMap :: HFunctor f => (f (Rec g v) ~> g (Rec g v)) -> Rec f v ~> Rec g v
graphMap f = cata $ \ rc -> case rc of
  Pure v -> var v
  Impure (Mu g) -> mu (f . g)
  Impure (In r) -> rec (f r)

agraphMap :: HFunctor f => (f (Rec g v) ~> g (Rec g v)) -> Rec f v ~> Cofree (FreeF (RecF f v) v) (Rec g v)
agraphMap f = acata $ \ rc -> case rc of
  Pure v -> var v
  Impure (Mu g) -> mu (f . g)
  Impure (In x) -> rec (f x)

liftRec :: (f (Rec f v) ~> f (Rec f v)) -> Rec f v ~> Rec f v
liftRec f rc = case unRec rc of
  Pure v -> var v
  Impure (Mu g) -> mu (f . g)
  Impure (In r) -> rec (f r)

pjoin :: HFunctor f => Rec f (Rec f v) ~> Rec f v
pjoin = cata $ \ rc -> case rc of
  Pure x -> x
  Impure (Mu g) -> mu (g . var)
  Impure (In r) -> rec r

preturn :: v ~> Rec f v
preturn = var

modifyGraph :: (forall v. Rec f v ~> Rec g v) -> Graph f ~> Graph g
modifyGraph f g = Graph (f (unGraph g))

unroll :: HFunctor f => Rec f (Rec f v) a -> Rec f (Rec f v) a
unroll = cata $ \ rc -> case rc of
  Pure v -> var v
  Impure (Mu g) -> rec (g (pjoin (unroll (mu g))))
  Impure (In r) -> rec r

unrollGraph :: HFunctor f => Graph f ~> Graph f
unrollGraph g = Graph (pjoin (unroll (unGraph g)))


-- Equality

eqRec :: HEqF f => Int -> Rec f (Const Int) a -> Rec f (Const Int) a -> Bool
eqRec n a b = case (unRec a, unRec b) of
  (Pure x, Pure y) -> x == y
  (Impure (Mu g), Impure (Mu h)) -> let a = g (Const (succ n))
                                        b = h (Const (succ n)) in
                                        heqF (eqRec (succ n)) a b
  (Impure (In x), Impure (In y)) -> heqF (eqRec n) x y
  _ -> False


-- Show

showsRec :: HShowF f => (forall b. [Const Char b]) -> Int -> Rec f (Const Char) a -> ShowS
showsRec s n rec = case unRec rec of
  Pure c -> showChar (getConst c)
  Impure (Mu g) -> let (a, s') = (head s, tail s) in
                       showString "Mu (\\ " . showChar (getConst a) . showString " ->\n  "
                       . hshowsPrecF n (showsRec (fmap (Const . getConst) s')) (g a) . showString "\n)\n"
  Impure (In fa) -> hshowsPrecF n (showsRec s) fa


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
          Mu g -> Mu (hfmap f . g)
          In r -> In (hfmap f r)

type instance Base (Rec f v) = FreeF (RecF f v) v

instance HFunctor f => Recursive (Rec f v) where project = unRec
instance HFunctor f => Corecursive (Rec f v) where embed = Rec

class HIsofunctor f
  where hisomap :: (a ~> b) -> (b ~> a) -> (f a z -> f b z, f b z -> f a z)

instance HFunctor f => HIsofunctor (Rec f)
  where hisomap :: forall a b z. (a ~> b) -> (b ~> a) -> (Rec f a z -> Rec f b z, Rec f b z -> Rec f a z)
        hisomap f g = (to, from)
          where to :: Rec f a ~> Rec f b
                to rc = case unRec rc of
                  Pure v -> var (f v)
                  Impure (Mu h) -> mu (hfmap to . h . g)
                  Impure (In r) -> rec (hfmap to r)
                from :: Rec f b ~> Rec f a
                from rc = case unRec rc of
                  Pure v -> var (g v)
                  Impure (Mu h) -> mu (hfmap from . h . f)
                  Impure (In r) -> rec (hfmap from r)

instance HFunctor f => HIsofunctor (RecF f v) where
  hisomap f g = (hfmap f, hfmap g)

instance HFunctor f => HProfunctor (RecF f) where
  hdimap f g rec = case rec of
    Mu h -> Mu (hfmap g . h . f)
    In r -> In (hfmap g r)
