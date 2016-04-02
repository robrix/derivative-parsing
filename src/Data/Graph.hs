{-# LANGUAGE RankNTypes #-}
module Data.Graph where

data Rec f v
  = Var v
  | Mu ([v] -> [f (Rec f v)])
  | In (f (Rec f v))

newtype Graph f = Down { up :: forall v. Rec f v }
