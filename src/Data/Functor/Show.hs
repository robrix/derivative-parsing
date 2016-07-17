module Data.Functor.Show (ShowF(..)) where

import Data.Functor.Const

class ShowF f
  where showsPrecF :: Int -> (Int -> a -> ShowS) -> f a -> ShowS


-- | A wrapper to enable use of an extant 'Show' instance when defining a 'ShowF' instance.
data Showable a = Showable !(Int -> a -> ShowS) !a


-- Instances

instance Show a => ShowF (Const a)
  where showsPrecF n _ = showsPrec n . getConst

instance ShowF []
  where showsPrecF n f = showsPrec n . fmap (Showable f)

instance Show (Showable a)
  where showsPrec n (Showable f a) = f n a
