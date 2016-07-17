module Data.Functor.Show (ShowF(..)) where

import Data.Functor.Const

class ShowF f
  where showsPrecF :: (Int -> a -> ShowS) -> Int -> f a -> ShowS


-- | A wrapper to enable use of an extant 'Show' instance when defining a 'ShowF' instance.
data Showable a = Showable !(Int -> a -> ShowS) !a


-- Instances

instance Show a => ShowF (Const a)
  where showsPrecF _ n = showsPrec n . getConst

instance ShowF []
  where showsPrecF f n = showsPrec n . fmap (Showable f)

instance Show (Showable a)
  where showsPrec n (Showable f a) = f n a
