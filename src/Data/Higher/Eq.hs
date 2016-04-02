module Data.Higher.Eq where

import Control.Applicative
import Data.Function

class HEq f where
  heq :: f a -> f a -> Bool

instance Eq a => HEq (Const a)
  where heq = (==) `on` getConst
