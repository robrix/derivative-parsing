{-# LANGUAGE FlexibleContexts, RankNTypes, TypeOperators #-}
module Data.Graph
( Graph(..)
, Rec(..)
, gfold
, fold
, cfold
, sfold
, transform
, gmap
, pjoin
, unrollGraph
, geq
) where

import Data.Bifunctor
import Data.Function
import Data.Functor.Eq
import Data.Functor.Show
import Data.Higher.Transformation

data Rec f v
  = Var v
  | Mu ([v] -> [f (Rec f v)])
  | In (f (Rec f v))

newtype Graph f = Down { up :: forall v. Rec f v }

gfold :: Functor f => (t -> c) -> (([t] -> [c]) -> c) -> (f c -> c) -> Graph f -> c
gfold var bind recur = trans . up
  where trans (Var v) = var v
        trans (Mu g) = bind (map (recur . fmap trans) . g)
        trans (In fa) = recur (fmap trans fa)

fold :: Functor f => (f c -> c) -> c -> Graph f -> c
fold alg k = gfold id (\ g -> head (g (repeat k))) alg

cfold :: Functor f => (f t -> t) -> Graph f -> t
cfold = gfold id (head . fix)

sfold :: (Eq t, Functor f) => (f t -> t) -> t -> Graph f -> t
sfold alg k = gfold id (\ g -> head . fixVal (repeat k) $ g) alg

fixVal :: Eq a => a -> (a -> a) -> a
fixVal v f = if v == v' then v else fixVal v' f
  where v' = f v

transform :: (Functor f, Functor g) => (f ~> g) -> Graph f -> Graph g
transform f x = Down (hmap (up x))
  where hmap (Var x) = Var x
        hmap (Mu g) = Mu (map (f . fmap hmap) . g)
        hmap (In x) = In (f (fmap hmap x))

gmap :: (Bifunctor f, Functor (f a), Functor (f b)) => (a -> b) -> Graph (f a) -> Graph (f b)
gmap f = transform (bimap f id)

pjoin :: Functor f => Rec f (Rec f a) -> Rec f a
pjoin (Var v) = v
pjoin (Mu g) = Mu (map (fmap pjoin) . g . map Var)
pjoin (In r) = In (fmap pjoin r)

unrollGraph :: Functor f => Graph f -> Graph f
unrollGraph g = Down (pjoin (unroll (up g)))

unroll :: Functor f => Rec f (Rec f a) -> Rec f (Rec f a)
unroll (Var v) = Var v
unroll (Mu g) = In (head (g (repeat (pjoin (Mu g)))))
unroll (In r) = In (fmap unroll r)

geq :: EqF f => Graph f -> Graph f -> Bool
geq a b = eqRec 0 (up a) (up b)

eqRec :: EqF f => Int -> Rec f Int -> Rec f Int -> Bool
eqRec _ (Var a) (Var b) = a == b
eqRec n (Mu g) (Mu h) = let a = g (iterate succ n)
                            b = h (iterate succ n) in
                            and $ zipWith (eqF (eqRec (n + length a))) a b
eqRec n (In a) (In b) = eqF (eqRec n) a b
eqRec _ _ _ = False

showGraph :: ShowF f => Graph f -> String
showGraph g = showRec (iterate succ 'a') (up g)

showRec :: ShowF f => String -> Rec f Char -> String
showRec _ (Var v) = [v]
showRec s (Mu f) = let r = f s
                       (fr, s') = splitAt (length r) s in
                       "Mu (\n)" ++ concat
                         ["  " ++ [a] ++ " => " ++ v ++ "\n"
                         | (a, v) <- zip fr (map (showF (showRec s')) r)] ++ "\n"
showRec s (In fa) = showF (showRec s) fa
