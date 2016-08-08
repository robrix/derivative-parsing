{-# LANGUAGE GADTs #-}
module Data.Predicate
( Predicate(..)
, satisfies
) where

import Data.Char

data Predicate t where
  Equal :: Eq t => t -> Predicate t
  Category :: GeneralCategory -> Predicate Char
  Constant :: Bool -> Predicate t


satisfies :: t -> Predicate t -> Bool
satisfies t p = case p of
  Equal t' -> t == t'
  Category c -> generalCategory t == c
  Constant c -> c


-- Instances

instance Eq (Predicate t) where
  Equal a == Equal b = a == b
  Category a == Category b = a == b
  Constant a == Constant b = a == b
  _ == _ = False

instance Show t => Show (Predicate t) where
  showsPrec n p = case p of
    Equal t -> showParen True $ showString "== " . showsPrec 4 t
    Category c -> showParen (n >= 9) $ showString "(== " . showsPrec 4 c . showString ") . generalCategory"
    Constant c -> showsPrec n c
