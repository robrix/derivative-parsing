{-# LANGUAGE RankNTypes #-}
module Data.Graph where

import Data.Function

data Rec f v
  = Var v
  | Mu (v -> f (Rec f v))
  | In (f (Rec f v))

newtype Graph f = Graph { unGraph :: forall v. Rec f v }

gfold :: Functor f => (v -> c) -> ((v -> c) -> c) -> (f c -> c) -> Graph f -> c
gfold var bind recur = grfold var bind recur . unGraph

grfold :: Functor f => (v -> c) -> ((v -> c) -> c) -> (f c -> c) -> Rec f v -> c
grfold var bind recur rec = case rec of
  Var x -> var x
  Mu g -> bind (recur . fmap (grfold var bind recur) . g)
  In fa -> recur (fmap (grfold var bind recur) fa)

fold :: Functor f => (f c -> c) -> c -> Graph f -> c
fold alg k = rfold alg k . unGraph

rfold :: Functor f => (f c -> c) -> c -> Rec f c -> c
rfold alg k = grfold id ($ k) alg

runfold :: Functor f => (c -> f c) -> c -> Rec f v
runfold coalgebra seed = In (fmap (runfold coalgebra) (coalgebra seed))

cfold :: Functor f => (f t -> t) -> Graph f -> t
cfold = gfold id fix

sfold :: (Functor f, Eq c) => (f c -> c) -> c -> Graph f -> c
sfold alg k = gfold id (fixVal k) alg

fixVal :: Eq a => a -> (a -> a) -> a
fixVal v f = if v == v' then v else fixVal v' f
  where v' = f v


class Isofunctor f
  where isomap :: (a -> b) -> (b -> a) -> (f a -> f b, f b -> f a)
