module Data.Graph where

data Rec f v
  = Var v
  | Mu ([v] -> [f (Rec f v)])
  | In (f (Rec f v))
