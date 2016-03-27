{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Data.Higher.Graph where

import Control.Applicative
import Data.Function
import Data.Higher.Functor

data HRec h v a
  = Var v
  | Mu (forall a. [v] -> [h (HRec h v) a])
  | In (h (HRec h v) a)

newtype HGraph h a = HDown { hup :: forall v. HRec h v a }

hgfold :: forall h v c. HFunctor h => (forall a. v -> c a) -> (forall a. (forall a. [v] -> [c a]) -> c a) -> (forall a. h c a -> c a) -> forall a. HGraph h a -> c a
hgfold var bind recur = trans . hup
  where trans :: HRec h v a -> c a
        trans (Var x) = var x
        trans (Mu g) = bind (map (recur . hfmap trans) . g)
        trans (In fa) = recur (hfmap trans fa)

fold :: HFunctor h => (forall b. h (Const a) b -> Const a b) -> a -> HGraph h b -> a
fold alg k = getConst . hgfold Const (\ g -> head (g (repeat k))) alg

cfold :: HFunctor h => (forall b. h (Const a) b -> Const a b) -> HGraph h b -> a
cfold alg = getConst . hgfold Const (\ g -> head . fix $ g . fmap getConst) alg

sfold :: (HFunctor h, Eq a) => (forall b. h (Const a) b -> Const a b) -> a -> HGraph h b -> a
sfold alg k = getConst . hgfold Const (\ g -> head . fixVal (repeat (Const k)) $ g . fmap getConst) alg

fixVal :: Eq a => a -> (a -> a) -> a
fixVal v f = if v == v' then v else fixVal v' f
  where v' = f v
