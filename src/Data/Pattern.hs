{-# LANGUAGE GADTs #-}
module Data.Pattern
( PatternF(..)
) where

import Data.Predicate

data PatternF t f a where
  Cat :: f a -> f b -> PatternF t f (a, b)
  Alt :: f a -> f a -> PatternF t f a
  Rep :: f a -> PatternF t f [a]
  Map :: (a -> b) -> f a -> PatternF t f b
  Bnd :: f a -> (a -> f b) -> PatternF t f b
  Sat :: Predicate t -> PatternF t f t
  Ret :: [a] -> PatternF t f a
  Nul :: PatternF t f a
  Lab :: f a -> String -> PatternF t f a
  Del :: f a -> PatternF t f a
