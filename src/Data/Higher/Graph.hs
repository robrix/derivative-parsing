{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeOperators #-}
module Data.Higher.Graph where

import Control.Applicative
import Data.Function
import Data.Higher.Functor
import Data.Higher.Transformation

data HRec h v a
  = Var v
  | Mu ([v] -> [h (HRec h v) a])
  | In (h (HRec h v) a)

newtype HGraph h a = HDown { hup :: forall v. HRec h v a }

gfold :: forall h v c. HFunctor h => (v -> c) -> (([v] -> [c]) -> c) -> (forall a. h (Const c) a -> c) -> forall a. HGraph h a -> c
gfold var bind recur = getConst . trans . hup
  where trans :: HRec h v a -> Const c a
        trans (Var x) = Const $ var x
        trans (Mu g) = Const $ bind (map (recur . hfmap trans) . g)
        trans (In fa) = Const $ recur (hfmap trans fa)

fold :: HFunctor h => (forall b. h (Const a) b -> a) -> a -> HGraph h b -> a
fold alg k = gfold id (\ g -> head (g (repeat k))) alg

cfold :: HFunctor h => (forall b. h (Const a) b -> a) -> HGraph h b -> a
cfold = gfold id (head . fix)

sfold :: (HFunctor h, Eq a) => (forall b. h (Const a) b -> a) -> a -> HGraph h b -> a
sfold alg k = gfold id (\ g -> head . fixVal (repeat k) $ g) alg

fixVal :: Eq a => a -> (a -> a) -> a
fixVal v f = if v == v' then v else fixVal v' f
  where v' = f v

hgfold :: forall h v c. HFunctor h => (forall a. v -> c a) -> (forall a. ([v] -> [c a]) -> c a) -> (h c ~> c) -> HGraph h ~> c
hgfold var bind recur = trans . hup
  where trans :: HRec h v a -> c a
        trans (Var x) = var x
        trans (Mu g) = bind (map (recur . hfmap trans) . g)
        trans (In fa) = recur (hfmap trans fa)
