module Data.Graph where

data Rec f a
  = Var a
  | Mu ([a] -> [f (Rec f a)])
  | In (f (Rec f a))

