{-# LANGUAGE InstanceSigs, RankNTypes, ScopedTypeVariables, TypeOperators #-}
module Data.Higher.Graph
( HRec(..)
, HGraph(..)
, hgfold
, hgrfold
, gfold
, hfold
, hrfold
, fold
, rfold
, hcfold
, cfold
, hsfold
, sfold
, hgcata
, transform
, hmap
, hpjoin
, modifyGraph
, unroll
, unrollGraph
) where

import Control.Applicative
import Data.Foldable (asum)
import Data.Function
import Data.Functor.Eq
import Data.Higher.Eq
import Data.Higher.Functor
import Data.Higher.Functor.Eq
import Data.Higher.Isofunctor
import Data.Higher.Functor.Show
import Data.Higher.Transformation

data HRec f v a
  = Var (v a)
  | Mu ([v a] -> [f (HRec f v) a])
  | In (f (HRec f v) a)

newtype HGraph f a = HDown { hup :: forall v. HRec f v a }


-- Folds

hgfold :: forall f v c. HFunctor f => (v ~> c) -> (forall a. ([v a] -> [c a]) -> c a) -> (f c ~> c) -> HGraph f ~> c
hgfold var bind recur = hgrfold var bind recur . hup

hgrfold :: HFunctor f => (v ~> c) -> (forall a. ([v a] -> [c a]) -> c a) -> (f c ~> c) -> HRec f v ~> c
hgrfold var bind recur rec = case rec of
  Var x -> var x
  Mu g -> bind (map (recur . hfmap (hgrfold var bind recur)) . g)
  In fa -> recur (hfmap (hgrfold var bind recur) fa)

gfold :: forall f v c a. HFunctor f => (forall a. v a -> c) -> (forall a. ([v a] -> [c]) -> c) -> (forall a. f (Const c) a -> c) -> HGraph f a -> c
gfold var bind recur = getConst . hgfold (Const . var) (Const . bind . (fmap getConst .)) (Const . recur)

hfold :: HFunctor f => (f c ~> c) -> (forall a. c a) -> HGraph f ~> c
hfold alg k = hrfold alg k . hup

hrfold :: HFunctor f => (f c ~> c) -> (forall a. c a) -> HRec f c ~> c
hrfold alg k = hgrfold id (\ g -> head (g (repeat k))) alg

fold :: HFunctor f => (forall b. f (Const a) b -> a) -> a -> HGraph f b -> a
fold alg k = getConst . hfold (Const . alg) (Const k)

rfold :: HFunctor f => (forall b. f (Const a) b -> a) -> a -> HRec f (Const a) b -> a
rfold alg k = getConst . hrfold (Const . alg) (Const k)

hcfold :: HFunctor f => (f t ~> t) -> HGraph f ~> t
hcfold = hgfold id (head . fix)

cfold :: HFunctor f => (forall b. f (Const a) b -> a) -> HGraph f b -> a
cfold alg = getConst . hcfold (Const . alg)

hsfold :: (HFunctor f, HEq c) => (f c ~> c) -> (forall a. c a) -> HGraph f ~> c
hsfold alg k = hgfold id (head . fhfixVal (repeat k)) alg

sfold :: (HFunctor f, Eq a) => (forall b. f (Const a) b -> a) -> a -> HGraph f b -> a
sfold alg k = getConst . hsfold (Const . alg) (Const k)

hgcata :: (HFunctor f, Alternative v) => (f v ~> v) -> HRec f v ~> v
hgcata f rec = case rec of
  Var v -> v
  Mu g -> asum . map (f . hfmap (hgcata f)) . g $ repeat empty
  In r -> f (hfmap (hgcata f) r)


-- Maps

transform :: forall f g. (HFunctor f, HFunctor g) => (forall v. f (HRec g v) ~> g (HRec g v)) -> HGraph f ~> HGraph g
transform f = modifyGraph (hmap f)

hmap :: (HFunctor f, HFunctor g) => (f (HRec g v) ~> g (HRec g v)) -> HRec f v ~> HRec g v
hmap f rec = case rec of
  Var x -> Var x
  Mu g -> Mu (map (f . hfmap (hmap f)) . g)
  In x -> In (f (hfmap (hmap f) x))

hpjoin :: HFunctor f => HRec f (HRec f v) a -> HRec f v a
hpjoin rec = case rec of
  Var x -> x
  Mu g -> Mu (map (hfmap hpjoin) . g . map Var)
  In r -> In (hfmap hpjoin r)

hpreturn :: v a -> HRec f v a
hpreturn = Var

modifyGraph :: (forall v. HRec f v ~> HRec g v) -> HGraph f ~> HGraph g
modifyGraph f g = HDown (f (hup g))

unroll :: HFunctor f => HRec f (HRec f v) a -> HRec f (HRec f v) a
unroll rec = case rec of
  Var v -> Var v
  Mu g -> In (head (g (repeat (hpjoin (unroll (Mu g))))))
  In r -> In (hfmap unroll r)

unrollGraph :: HFunctor f => HGraph f ~> HGraph f
unrollGraph g = HDown (hpjoin (unroll (hup g)))

-- Equality

eqRec :: HEqF f => Int -> HRec f (Const Int) a -> HRec f (Const Int) a -> Bool
eqRec n a b = case (a, b) of
  (Var x, Var y) -> x == y
  (Mu g, Mu h) -> let a = g (iterate (modifyConst succ) (Const n))
                      b = h (iterate (modifyConst succ) (Const n)) in
                      and $ zipWith (heqF (eqRec (n + length a))) a b
  (In x, In y) -> heqF (eqRec n) x y
  _ -> False


-- Show

showsRec :: HShowF f => (forall b. [Const Char b]) -> Int -> HRec f (Const Char) a -> ShowS
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

instance HEqF f => Eq (HGraph f a)
  where a == b = eqRec 0 (hup a) (hup b)

instance HShowF f => Show (HGraph f a)
  where showsPrec n = showsRec (iterate (modifyConst succ) (Const 'a')) n . hup

instance HFunctor f => HIsofunctor (HRec f)
  where hisomap :: forall c d. (c ~> d) -> (d ~> c) -> forall a. (HRec f c a -> HRec f d a, HRec f d a -> HRec f c a)
        hisomap f g = (s, t)
          where s :: forall a. HRec f c a -> HRec f d a
                s rec = case rec of
                  Var v -> Var (f v)
                  Mu h -> Mu (map (hfmap s) . h . map g)
                  In r -> In (hfmap s r)
                t :: forall a. HRec f d a -> HRec f c a
                t rec = case rec of
                  Var v -> Var (g v)
                  Mu h -> Mu (map (hfmap t) . h . map f)
                  In r -> In (hfmap t r)
