{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Data.Higher.Graph where

import Data.Higher.Functor

data HRec h v a
  = Var (v a)
  | Mu (forall a. [v a] -> [h (HRec h v) a])
  | In (h (HRec h v) a)

newtype HGraph h a = HDown { hup :: forall v. HRec h v a }

hgfold :: forall h v c. HFunctor h => (forall a. v a -> c a) -> (forall a. (forall a. [v a] -> [c a]) -> c a) -> (forall a. h c a -> c a) -> forall a. HGraph h a -> c a
hgfold var bind recur = trans . hup
  where trans :: HRec h v a -> c a
        trans (Var x) = var x
        trans (Mu g) = bind (map (recur . hfmap trans) . g)
        trans (In fa) = recur (hfmap trans fa)
