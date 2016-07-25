{-# LANGUAGE FlexibleInstances, PolyKinds #-}
module Data.Higher.Fix
( Fix(..)
) where

import Data.Higher.Functor.Recursive


-- Types

newtype Fix f v a = Fix { unFix :: f (Fix f v) a }


-- Instances

instance HRecursive Fix where hproject = unFix
