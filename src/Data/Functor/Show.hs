module Data.Functor.Show where

import Data.Functor.Const

class ShowF f
  where showsPrecF :: Int -> (Int -> a -> ShowS) -> f a -> ShowS

instance Show a => ShowF (Const a)
  where showsPrecF n _ = showsPrec n . getConst
