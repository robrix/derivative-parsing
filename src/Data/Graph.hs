{-# LANGUAGE RankNTypes #-}
module Data.Graph where

import Data.Bifunctor
import Data.Function
import Data.Functor.Show

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

cfold :: Functor f => (f t -> t) -> Graph f -> t
cfold = gfold id fix

sfold :: (Functor f, Eq c) => (f c -> c) -> c -> Graph f -> c
sfold alg k = gfold id (fixVal k) alg

fixVal :: Eq a => a -> (a -> a) -> a
fixVal v f = if v == v' then v else fixVal v' f
  where v' = f v


transform :: Functor f => (forall v. f (Rec g v) -> g (Rec g v)) -> Graph f -> Graph g
transform f = modifyGraph (rmap f)

rmap :: Functor f => (forall v. f (Rec g v) -> g (Rec g v)) -> Rec f v -> Rec g v
rmap f rec = case rec of
  Var x -> Var x
  Mu g -> Mu (f . fmap (rmap f) . g)
  In x -> In (f (fmap (rmap f) x))

liftRec :: (forall v. f (Rec f v) -> f (Rec f v)) -> Rec f v -> Rec f v
liftRec f rec = case rec of
  Var v -> Var v
  Mu g -> Mu (f . g)
  In r -> In (f r)

pjoin :: Functor f => Rec f (Rec f v) -> Rec f v
pjoin rec = case rec of
  Var x -> x
  Mu g -> Mu (fmap pjoin . g . Var)
  In r -> In (fmap pjoin r)

gmap :: (Bifunctor f, Functor (f a)) => (a -> b) -> Graph (f a) -> Graph (f b)
gmap f = transform (first f)

preturn :: v -> Rec f v
preturn = Var

modifyGraph :: (forall v. Rec f v -> Rec g v) -> Graph f -> Graph g
modifyGraph f g = Graph (f (unGraph g))

unroll :: Functor f => Rec f (Rec f v) -> Rec f (Rec f v)
unroll rec = case rec of
  Var v -> Var v
  Mu g -> In (g (pjoin (unroll (Mu g))))
  In r -> In (fmap unroll r)

unrollGraph :: Functor f => Graph f -> Graph f
unrollGraph g = Graph (pjoin (unroll (unGraph g)))


-- Equality

eqRec :: EqF f => Int -> Rec f Int -> Rec f Int -> Bool
eqRec n a b = case (a, b) of
  (Var x, Var y) -> x == y
  (Mu g, Mu h) -> let a = g (succ n)
                      b = h (succ n) in
                      eqF (eqRec (succ n)) a b
  (In x, In y) -> eqF (eqRec n) x y
  _ -> False


-- Show

showsRec :: ShowF f => String -> Int -> Rec f Char -> ShowS
showsRec s n rec = case rec of
  Var c -> showChar c
  Mu g -> let (a, s') = (head s, tail s) in
              showString "Mu (\n  " . showChar a . showString " => "
              . showsPrecF n (showsRec s') (g a) . showString "\n)\n"
  In r -> showsPrecF n (showsRec s) r


class Functor f => EqF f
  where eqF :: (r -> r -> Bool) -> f r -> f r -> Bool

instance EqF f => Eq (Graph f)
  where (==) = eqRec 0 `on` unGraph

instance ShowF f => Show (Graph f)
  where showsPrec n = showsRec (iterate succ 'a') n . unGraph

class Isofunctor f
  where isomap :: (a -> b) -> (b -> a) -> (f a -> f b, f b -> f a)

instance Functor f => Isofunctor (Rec f)
  where isomap f g = (into, outof)
          where into rec = case rec of
                  Var v -> Var (f v)
                  Mu h -> Mu (fmap into . h . g)
                  In r -> In (into <$> r)
                outof rec = case rec of
                  Var v -> Var (g v)
                  Mu h -> Mu (fmap outof . h . f)
                  In r -> In (outof <$> r)
