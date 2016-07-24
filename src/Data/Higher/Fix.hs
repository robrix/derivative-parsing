module Data.Higher.Fix
( Fix(..)
) where

newtype Fix f a = Fix { unFix :: f (Fix f) a }
