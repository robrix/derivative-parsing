{-# LANGUAGE FlexibleInstances, InstanceSigs, PolyKinds, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Data.Higher.Graph
( Rec
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
, giter
, transform
, graphMap
, liftRec
, pjoin
, modifyGraph
, unroll
, unrollGraph
) where

import Control.Higher.Monad.Free
import Data.Function
import Data.Functor.Const
import Data.Higher.Eq
import Data.Higher.Functor
import Data.Higher.Functor.Eq
import Data.Higher.Functor.Show
import Data.Higher.Transformation

data RecF f v b a
  = Mu (v a -> f b a)
  | In (f b a)

type Rec f v = Free (RecF f v) v

newtype Graph f a = Graph { unGraph :: forall v. Rec f v a }


-- Smart constructors

var :: v a -> Rec f v a
var = free . Pure

mu :: (v a -> f (Rec f v) a) -> Rec f v a
mu = free . Impure . Mu

rec :: f (Rec f v) a -> Rec f v a
rec = free . Impure . In


-- Folds

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

cfold :: HFunctor f => (f t ~> t) -> Graph f ~> t
cfold = gfold id fix

sfold :: (HFunctor f, HEq c) => (f c ~> c) -> (forall a. c a) -> Graph f ~> c
sfold alg k = gfold id (fixVal k) alg

giter :: HFunctor f => (a ~> b) -> (RecF f a b ~> b) -> Graph f ~> b
giter f alg = iter f alg . unGraph


-- Maps

transform :: HFunctor f => (forall v. f (Rec g v) ~> g (Rec g v)) -> Graph f ~> Graph g
transform f = modifyGraph (graphMap f)

graphMap :: HFunctor f => (f (Rec g a) ~> g (Rec g a)) -> Rec f a ~> Rec g a
graphMap f = iter var $ \ rc -> case rc of
  Mu g -> mu (f . g)
  In r -> rec (f r)

liftRec :: (f (Rec f v) ~> g (Rec g v)) -> Rec f v ~> Rec g v
liftRec f rc = case runFree rc of
  Pure v -> var v
  Impure (Mu g) -> mu (f . g)
  Impure (In r) -> rec (f r)

pjoin :: HFunctor f => Rec f (Rec f v) ~> Rec f v
pjoin = iter id $ \ rc -> case rc of
  Mu g -> mu (g . var)
  In r -> rec r

preturn :: v ~> Rec f v
preturn = var

modifyGraph :: (forall v. Rec f v ~> Rec g v) -> Graph f ~> Graph g
modifyGraph f g = Graph (f (unGraph g))

unroll :: HFunctor f => Rec f (Rec f v) a -> Rec f (Rec f v) a
unroll = iter var $ \ rc -> case rc of
  Mu g -> rec (g (pjoin (unroll (mu g))))
  In r -> rec r

unrollGraph :: HFunctor f => Graph f ~> Graph f
unrollGraph g = Graph (pjoin (unroll (unGraph g)))


-- Equality

eqRec :: HEqF f => Int -> Rec f (Const Int) a -> Rec f (Const Int) a -> Bool
eqRec n a b = case (runFree a, runFree b) of
  (Pure x, Pure y) -> x == y
  (Impure (Mu g), Impure (Mu h)) -> let a = g (Const (succ n))
                                        b = h (Const (succ n)) in
                                        heqF (eqRec (succ n)) a b
  (Impure (In x), Impure (In y)) -> heqF (eqRec n) x y
  _ -> False


-- Show

showsRec :: HShowF f => String -> Int -> Rec f (Const Char) a -> ShowS
showsRec s n rec = case runFree rec of
  Pure c -> showChar (getConst c)
  Impure (Mu g) -> showString "Mu (\\ " . showChar (head s) . showString " ->\n  "
                   . hshowsPrecF (showsRec (tail s)) n (g (Const (head s))) . showString "\n)\n"
  Impure (In fa) -> hshowsPrecF (showsRec s) n fa

showsRecF :: HShowF f => Char -> Int -> RecF f (Const Char) (Const Char) a -> ShowS
showsRecF c n rec = case rec of
  Mu g -> showString "Mu (\\ " . showChar c . showString " ->\n  "
          . hshowsPrecF showsPrec n (g (Const c)) . showString "\n)\n"
  In fa -> hshowsPrecF showsPrec n fa


-- Implementation details

fixVal :: HEq h => h a -> (h a -> h a) -> h a
fixVal v f = if v `heq` v' then v else fixVal v' f
  where v' = f v


-- Instances

instance HEqF f => Eq (Graph f a)
  where a == b = eqRec 0 (unGraph a) (unGraph b)

instance HShowF f => Show (Graph f a)
  where showsPrec n = showsPrec n . (unGraph :: Graph f ~> Rec f (Const Char))

instance HShowF f => Show (Rec f (Const Char) a)
  where showsPrec = showsRec (iterate succ 'a')

instance HFunctor f => HFunctor (RecF f v)
  where hfmap f rec = case rec of
          Mu g -> Mu (hfmap f . g)
          In r -> In (hfmap f r)
