{-# LANGUAGE RankNTypes #-}
module Data.Higher.Graph where

data HRec h v a
  = Var v
  | Mu ([v] -> [h (HRec h v) a])
  | In (h (HRec h v) a)

newtype HGraph h a = Down { up :: forall v. HRec h v a }

