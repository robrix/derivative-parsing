{-# LANGUAGE TypeFamilies #-}
module Data.Higher.Fix
( Fix(..)
) where

import Data.Higher.Functor
import Data.Higher.Functor.Recursive


-- Types

newtype Fix f a = Fix { unFix :: f (Fix f) a }


-- Instances

type instance Base (Fix f) = f

instance HFunctor f => HRecursive (Fix f) where hproject = unFix
