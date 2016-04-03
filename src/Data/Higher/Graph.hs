{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeOperators #-}
module Data.Higher.Graph
( HRec(..)
, HGraph(..)
, hfold
, fold
, hcfold
, cfold
, hsfold
, sfold
, transform
, hgmap
, hpjoin
) where

import Control.Applicative
import Data.Function
import Data.Functor.Eq
import Data.Higher.Bifunctor
import Data.Higher.Eq
import Data.Higher.Functor
import Data.Higher.Functor.Eq
import Data.Higher.Functor.Show
import Data.Higher.Transformation

data HRec h v a
  = Var (v a)
  | Mu ([v a] -> [h (HRec h v) a])
  | In (h (HRec h v) a)

newtype HGraph h a = HDown { hup :: forall v. HRec h v a }


-- Folds

hgfold :: forall f v c. HFunctor f => (v ~> c) -> (forall a. ([v a] -> [c a]) -> c a) -> (f c ~> c) -> HGraph f ~> c
hgfold var bind recur = trans . hup
  where trans :: forall a. HRec f v a -> c a
        trans (Var x) = var x
        trans (Mu g) = bind (map (recur . hfmap trans) . g)
        trans (In fa) = recur (hfmap trans fa)

gfold :: forall h v c a. HFunctor h => (forall a. v a -> c) -> (forall a. ([v a] -> [c]) -> c) -> (forall a. h (Const c) a -> c) -> HGraph h a -> c
gfold var bind recur = getConst . hgfold (Const . var) (Const . bind . (fmap getConst .)) (Const . recur)

hfold :: HFunctor h => (h c ~> c) -> (forall a. c a) -> HGraph h a -> c a
hfold alg k = hgfold id (\ g -> head (g (repeat k))) alg

fold :: HFunctor h => (forall b. h (Const a) b -> a) -> a -> HGraph h b -> a
fold alg k = getConst . hfold (Const . alg) (Const k)

hcfold :: HFunctor h => (h t ~> t) -> HGraph h ~> t
hcfold = hgfold id (head . fix)

cfold :: HFunctor h => (forall b. h (Const a) b -> a) -> HGraph h b -> a
cfold alg = getConst . hcfold (Const . alg)

hsfold :: (HFunctor f, HEq c) => (f c ~> c) -> (forall a. c a) -> HGraph f a -> c a
hsfold alg k = hgfold id (head . fhfixVal (repeat k)) alg

sfold :: (HFunctor h, Eq a) => (forall b. h (Const a) b -> a) -> a -> HGraph h b -> a
sfold alg k = getConst . hsfold (Const . alg) (Const k)


-- Maps

transform :: forall f g. (HFunctor f, HFunctor g) => (forall h. f h ~> g h) -> HGraph f ~> HGraph g
transform f x = HDown (hmap (hup x))
  where hmap :: HRec f v ~> HRec g v
        hmap (Var x) = Var x
        hmap (Mu g) = Mu (map (f . hfmap hmap) . g)
        hmap (In x) = In (f (hfmap hmap x))

hgmap :: (HBifunctor f, HFunctor (f a), HFunctor (f b)) => (a ~> b) -> HGraph (f a) ~> HGraph (f b)
hgmap f = transform (hfirst f)

hpjoin :: HFunctor f => HRec f (HRec f v) a -> HRec f v a
hpjoin (Var x) = x
hpjoin (Mu g) = Mu (map (hfmap hpjoin) . g . map Var)
hpjoin (In r) = In (hfmap hpjoin r)


-- Equality

instance HEqF f => Eq (HGraph f a)
  where a == b = eqRec 0 (hup a) (hup b)

eqRec :: HEqF f => Int -> HRec f (Const Int) a -> HRec f (Const Int) a -> Bool
eqRec n a b = case (a, b) of
  (Var x, Var y) -> x == y
  (Mu g, Mu h) -> let a = g (iterate (modifyConst succ) (Const n))
                      b = h (iterate (modifyConst succ) (Const n)) in
                      and $ zipWith (heqF (eqRec (n + length a))) a b
  (In x, In y) -> heqF (eqRec n) x y
  _ -> False


-- Show

instance HShowF f => Show (HGraph f a)
  where showsPrec n = showsRec (iterate (modifyConst succ) (Const 'a')) n . hup

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
