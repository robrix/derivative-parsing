{-# LANGUAGE FlexibleContexts, RankNTypes, TypeOperators #-}
module Data.Graph where

import Data.Bifunctor
import Data.Function
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
