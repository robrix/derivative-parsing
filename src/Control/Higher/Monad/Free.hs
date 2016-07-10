module Control.Higher.Monad.Free where

data Free p v a
  = Impure (p v (Free p v) a)
  | Pure (v a)
