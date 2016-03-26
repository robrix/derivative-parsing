{-# LANGUAGE RankNTypes #-}
module Data.Higher.Graph where

data HRec f v a
  = Var v
  | Mu ([v] -> [f (HRec f v) a])
  | In (f (HRec f v) a)

newtype HGraph f a = Down { up :: forall v. HRec f v a }
