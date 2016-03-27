{-# LANGUAGE RankNTypes #-}
module Data.Higher.Graph where

data HRec h v a
  = Var (v a)
  | Mu (forall a. [v a] -> [h (HRec h v) a])
  | In (h (HRec h v) a)

newtype HGraph h a = HDown { hup :: forall v. HRec h v a }

