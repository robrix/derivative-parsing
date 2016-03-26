{-# LANGUAGE RankNTypes #-}
module Data.Graph where

data Rec f a
  = Var a
  | Mu ([a] -> [f (Rec f a)])
  | In (f (Rec f a))

newtype Graph f = Down { up :: forall a. Rec f a }
