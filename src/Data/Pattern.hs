{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses #-}
module Data.Pattern
( PatternF(..)
) where

import Control.Applicative
import Data.Predicate
import Data.Higher.Foldable
import Data.Higher.Functor
import Data.Higher.Functor.Eq
import Data.Higher.Functor.Show
import Data.Higher.Graph

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


-- Instances

instance HFunctor (PatternF t) where
  hfmap f p = case p of
    Cat a b -> Cat (f a) (f b)
    Alt a b -> Alt (f a) (f b)
    Rep a -> Rep (f a)
    Map g p -> Map g (f p)
    Bnd p g -> Bnd (f p) (f . g)
    Sat p -> Sat p
    Ret as -> Ret as
    Nul -> Nul
    Lab p s -> Lab (f p) s
    Del a -> Del (f a)

instance (Alternative a, Monad a) => HFoldable (PatternF t) a where
  hfoldMap f p = case p of
    Cat a b -> f ((,) <$> a <*> b)
    Alt a b -> f (a <|> b)
    Map g p -> f (g <$> p)
    Bnd p g -> f (p >>= g)
    Lab p _ -> f p
    Del a -> f a
    _ -> empty

instance HEqF (PatternF t)
  where heqF eq a b = case (a, b) of
          (Cat a1 b1, Cat a2 b2) -> eq a1 a2 && eq b1 b2
          (Alt a1 b1, Alt a2 b2) -> eq a1 a2 && eq b1 b2
          -- (Map f1 p1, Map f2 p2) -> eq p1 p2
          -- (Bnd p1 f1, Bnd p2 f2) -> eq p1 p2
          (Sat p1, Sat p2) -> p1 == p2
          (Ret r1, Ret r2) -> length r1 == length r2
          (Nul, Nul) -> True
          (Lab p1 s1, Lab p2 s2) -> s1 == s2 && eq p1 p2
          _ -> False

instance Show t => HShowF (PatternF t)
  where hshowsPrecF showsPrec n p = case p of
          Cat a b -> showParen (n > 4) $ showsPrec 4 a . showString " `cat` " . showsPrec 5 b
          Alt a b -> showParen (n > 3) $ showsPrec 3 a . showString " <|> " . showsPrec 4 b
          Rep a -> showParen (n >= 10) $ showString "many " . showsPrec 10 a
          Map _ p -> showParen (n > 4) $ showString "f <$> " . showsPrec 5 p
          Bnd p _ -> showParen (n > 1) $ showsPrec 1 p . showString " >>= f"
          Sat (Equal c) -> showParen (n >= 10) $ showString "char " . shows c
          Sat (Category c) -> showParen (n >= 10) $ showString "category " . shows c
          Sat (Constant _) -> showString "anyToken"
          Ret [_] -> showParen (n >= 10) $ showString "pure t"
          Ret t -> showString "ret [" . showIndices (length t) . showString "]"
          Nul -> showString "empty"
          Lab p s -> showParen (n > 2) $ showsPrec 3 p . showString " `label` " . shows s
          Del a -> showParen (n >= 10) $ showString "delta " . showsPrec 10 a
          where showIndices n = foldr (.) id ((showChar 't' .) . shows <$> take n (iterate succ (0 :: Integer)))


instance Functor (Rec (PatternF t) v)
  where fmap f = rec . Map f

instance Applicative (Rec (PatternF t) v) where
  pure = rec . Ret . pure
  a <*> b = uncurry ($) <$> rec (Cat a b)

instance Alternative (Rec (PatternF t) v) where
  empty = rec Nul
  a <|> b = rec (Alt a b)
  some v = (:) <$> v <*> many v
  many = rec . Rep

instance Monad (Rec (PatternF t) v) where
  return = pure
  (>>=) = (rec .) . Bnd