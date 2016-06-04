{-# LANGUAGE InstanceSigs, RankNTypes, ScopedTypeVariables, TypeOperators #-}
module Data.Higher.Graph
( Rec(..)
, Graph(..)
, gfold
, grfold
, fold
, rfold
, cfold
, sfold
, gcata
, transform
, graphMap
, liftRec
, pjoin
, modifyGraph
, unroll
, unrollGraph
) where

import Control.Applicative
import Data.Foldable (asum)
import Data.Function
import Data.Functor.Eq
import Data.Higher.Bifunctor
import Data.Higher.Eq
import Data.Higher.Functor
import Data.Higher.Functor.Eq
import Data.Higher.Isofunctor
import Data.Higher.Functor.Show
import Data.Higher.Transformation

data Rec f v a
  = Var (v a)
  | Mu ([v a] -> [f (Rec f v) a])
  | In (f (Rec f v) a)

newtype Graph f a = Graph { unGraph :: forall v. Rec f v a }


-- Folds

gfold :: forall f v c. HFunctor f => (v ~> c) -> (forall a. ([v a] -> [c a]) -> c a) -> (f c ~> c) -> Graph f ~> c
gfold var bind recur = grfold var bind recur . unGraph

grfold :: HFunctor f => (v ~> c) -> (forall a. ([v a] -> [c a]) -> c a) -> (f c ~> c) -> Rec f v ~> c
grfold var bind recur rec = case rec of
  Var x -> var x
  Mu g -> bind (map (recur . hfmap (grfold var bind recur)) . g)
  In fa -> recur (hfmap (grfold var bind recur) fa)

fold :: HFunctor f => (f c ~> c) -> (forall a. c a) -> Graph f ~> c
fold alg k = rfold alg k . unGraph

rfold :: HFunctor f => (f c ~> c) -> (forall a. c a) -> Rec f c ~> c
rfold alg k = grfold id (\ g -> head (g (repeat k))) alg

cfold :: HFunctor f => (f t ~> t) -> Graph f ~> t
cfold = gfold id (head . fix)

sfold :: (HFunctor f, HEq c) => (f c ~> c) -> (forall a. c a) -> Graph f ~> c
sfold alg k = gfold id (head . fhfixVal (repeat k)) alg

gcata :: (HFunctor f, Alternative v) => (f v ~> v) -> Rec f v ~> v
gcata f rec = case rec of
  Var v -> v
  Mu g -> asum . map (f . hfmap (gcata f)) . g $ repeat empty
  In r -> f (hfmap (gcata f) r)


-- Maps

transform :: HFunctor f => (forall v. f (Rec g v) ~> g (Rec g v)) -> Graph f ~> Graph g
transform f = modifyGraph (graphMap f)

graphMap :: HFunctor f => (f (Rec g v) ~> g (Rec g v)) -> Rec f v ~> Rec g v
graphMap f rec = case rec of
  Var x -> Var x
  Mu g -> Mu (map (f . hfmap (graphMap f)) . g)
  In x -> In (f (hfmap (graphMap f) x))

liftRec :: (f (Rec f v) ~> f (Rec f v)) -> Rec f v ~> Rec f v
liftRec f rec = case rec of
  Var v -> Var v
  Mu g -> Mu (map f . g)
  In r -> In (f r)

pjoin :: HFunctor f => Rec f (Rec f v) ~> Rec f v
pjoin rec = case rec of
  Var x -> x
  Mu g -> Mu (map (hfmap pjoin) . g . map Var)
  In r -> In (hfmap pjoin r)

gmap :: (HBifunctor f, HFunctor (f a)) => (a ~> b) -> Graph (f a) ~> Graph (f b)
gmap f = transform (hbimap f id)

preturn :: v ~> Rec f v
preturn = Var

modifyGraph :: (forall v. Rec f v ~> Rec g v) -> Graph f ~> Graph g
modifyGraph f g = Graph (f (unGraph g))

unroll :: HFunctor f => Rec f (Rec f v) a -> Rec f (Rec f v) a
unroll rec = case rec of
  Var v -> Var v
  Mu g -> In (head (g (repeat (pjoin (unroll (Mu g))))))
  In r -> In (hfmap unroll r)

unrollGraph :: HFunctor f => Graph f ~> Graph f
unrollGraph g = Graph (pjoin (unroll (unGraph g)))


-- Equality

eqRec :: HEqF f => Int -> Rec f (Const Int) a -> Rec f (Const Int) a -> Bool
eqRec n a b = case (a, b) of
  (Var x, Var y) -> x == y
  (Mu g, Mu h) -> let a = g (iterate (modifyConst succ) (Const n))
                      b = h (iterate (modifyConst succ) (Const n)) in
                      and $ zipWith (heqF (eqRec (n + length a))) a b
  (In x, In y) -> heqF (eqRec n) x y
  _ -> False


-- Show

showsRec :: HShowF f => (forall b. [Const Char b]) -> Int -> Rec f (Const Char) a -> ShowS
showsRec s n rec = case rec of
  Var c -> showChar (getConst c)
  Mu f -> let r = f s
              (fr, s') = splitAt (length r) s in
              showString "Mu (\n" . foldr (.) id
                [ showString "  " . showChar (getConst a) . showString " => " . v . showString "\n"
                | (a, v) <- zip fr (map (hshowsPrecF n (showsRec s')) r) ] . showString ")\n"
  In fa -> hshowsPrecF n (showsRec s) fa


-- Implementation details

fhfixVal :: (EqF f, HEq h) => f (h a) -> (f (h a) -> f (h a)) -> f (h a)
fhfixVal v f = if eqF heq v v' then v else fhfixVal v' f
  where v' = f v

modifyConst :: (a -> b) -> Const a ~> Const b
modifyConst f = Const . f . getConst


-- Instances

instance HEqF f => Eq (Graph f a)
  where a == b = eqRec 0 (unGraph a) (unGraph b)

instance HShowF f => Show (Graph f a)
  where showsPrec n = showsRec (iterate (modifyConst succ) (Const 'a')) n . unGraph

instance HFunctor f => HIsofunctor (Rec f)
  where hisomap :: forall c d. (c ~> d) -> (d ~> c) -> forall a. (Rec f c a -> Rec f d a, Rec f d a -> Rec f c a)
        hisomap f g = (s, t)
          where s :: Rec f c ~> Rec f d
                s rec = case rec of
                  Var v -> Var (f v)
                  Mu h -> Mu (map (hfmap s) . h . map g)
                  In r -> In (hfmap s r)
                t :: Rec f d ~> Rec f c
                t rec = case rec of
                  Var v -> Var (g v)
                  Mu h -> Mu (map (hfmap t) . h . map f)
                  In r -> In (hfmap t r)
