{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses #-}
module Data.Pattern
( PatternF(..)
, Pattern(..)
, commaSep1
, commaSep
, sep1
, sep
, oneOf
, cat
, char
, token
, category
, match
, delta
, ret
, label
, string
, anyToken
, compactF
, isTerminal
) where

import Control.Applicative
import Data.Bifunctor (first)
import Data.Char
import Data.Higher.Foldable
import Data.Higher.Functor
import Data.Higher.Functor.Eq
import Data.Higher.Functor.Fix
import Data.Higher.Functor.Recursive
import Data.Higher.Functor.Show
import Data.Higher.Graph
import qualified Data.Monoid as Monoid
import Data.Monoid hiding (Alt)
import Data.Predicate

-- Types

data PatternF t f a where
  Cat :: f a -> f b -> PatternF t f (a, b)
  Alt :: f a -> f a -> PatternF t f a
  Rep :: f a -> PatternF t f [a]
  Map :: (a -> b) -> f a -> PatternF t f b
  Bnd :: f a -> (a -> f b) -> PatternF t f b
  Sat :: Predicate t -> PatternF t f t
  Mat :: (t -> Maybe u) -> PatternF t f u
  Ret :: [a] -> PatternF t f a
  Nul :: PatternF t f a
  Lab :: f a -> String -> PatternF t f a
  Del :: f a -> PatternF t f a


-- Smart constructors

commaSep1 :: (Alternative r, Pattern r Char) => r a -> r [a]
commaSep1 = sep1 (char ',')

commaSep :: (Alternative r, Pattern r Char) => r a -> r [a]
commaSep = sep (char ',')

sep1 :: Alternative r => r sep -> r a -> r [a]
sep1 s p = (:) <$> p <*> many (s *> p)

sep :: Alternative r => r sep -> r a -> r [a]
sep s p = s `sep1` p <|> pure []

oneOf :: (Foldable t, Alternative f) => t (f a) -> f a
oneOf = getAlt . foldMap Monoid.Alt

infixl 4 `cat`

cat :: Pattern r t => r a -> r b -> r (a, b)
cat a = hembed . Cat a

char :: Pattern r Char => Char -> r Char
char = token

token :: (Eq t, Pattern r t) => t -> r t
token = hembed . Sat . Equal

category :: Pattern r Char => GeneralCategory -> r Char
category = hembed . Sat . Category

match :: Pattern r t => (t -> Maybe u) -> r u
match = hembed . Mat

delta :: Pattern r t => r a -> r a
delta = hembed . Del

ret :: Pattern r t => [a] -> r a
ret = hembed . Ret

infixr 2 `label`

label :: Pattern r t => r a -> String -> r a
label p = hembed . Lab p

string :: (Applicative r, Pattern r Char) => String -> r String
string string = sequenceA (hembed . Sat . Equal <$> string)

anyToken :: Pattern r t => r t
anyToken = hembed (Sat (Constant True))


-- API

compactF :: Pattern r t => PatternF t r a -> PatternF t r a
compactF p = case p of
  Cat a _ | Just Nul <- pattern a -> Nul
  Cat _ b | Just Nul <- pattern b -> Nul
  Cat l r | Just (Ret [t]) <- pattern l, Just b <- pattern r -> ((,) t) <$> b
  Cat l r | Just a <- pattern l, Just (Ret [t]) <- pattern r -> flip (,) t <$> a
  Cat l c | Just (Cat a b) <- pattern l -> (\ (a, (b, c)) -> ((a, b), c)) <$> Cat a (hembed (Cat b c))
  Cat l b | Just (Map f a) <- pattern l -> first f <$> Cat a b
  Alt a r | Just Nul <- pattern a, Just b <- pattern r -> b
  Alt a b | Just p <- pattern a, Just Nul <- pattern b -> p
  Alt l r | Just (Ret a) <- pattern l, Just (Ret b) <- pattern r -> Ret (a <> b)
  Rep a | Just Nul <- pattern a -> Ret [[]]
  Map f p | Just (Ret as) <- pattern p -> Ret (f <$> as)
  Map g a | Just (Map f p) <- pattern a -> Map (g . f) p
  Map _ p | Just Nul <- pattern p -> Nul
  Lab p _ | Just Nul <- pattern p -> Nul
  Lab p _ | Just (Ret t) <- pattern p -> Ret t
  Lab a _ | Just (Del p) <- pattern a -> Del p
  Del p | Just Nul <- pattern p -> Nul
  Del a | Just (Del p) <- pattern a -> Del p
  Del p | Just (Ret a) <- pattern p -> Ret a
  Del a | Just p <- pattern a, isTerminal p -> Nul
  a -> a

isTerminal :: PatternF t f a -> Bool
isTerminal p = case p of
  Cat _ _ -> False
  Alt _ _ -> False
  Rep _ -> False
  Map _ _ -> False
  Bnd _ _ -> False
  Lab _ _ -> False
  _ -> True


-- Classes

class (HCorecursive r, Base r ~ PatternF t) => Pattern r t
  where pattern :: r a -> Maybe (Base r r a)


-- Instances

instance HFunctor (PatternF t) where
  hfmap f p = case p of
    Cat a b -> Cat (f a) (f b)
    Alt a b -> Alt (f a) (f b)
    Rep a -> Rep (f a)
    Map g p -> Map g (f p)
    Bnd p g -> Bnd (f p) (f . g)
    Sat p -> Sat p
    Mat p -> Mat p
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
          Mat _ -> showParen (n >= 10) $ showString "match f"
          Ret [_] -> showParen (n >= 10) $ showString "pure t"
          Ret t -> showString "ret [" . showIndices (length t) . showString "]"
          Nul -> showString "empty"
          Lab p s -> showParen (n > 2) $ showsPrec 3 p . showString " `label` " . shows s
          Del a -> showParen (n >= 10) $ showString "delta " . showsPrec 10 a
          where showIndices n = foldr (.) id ((showChar 't' .) . shows <$> take n (iterate succ (0 :: Integer)))

instance Pattern r t => Functor (PatternF t r)
  where fmap f p = compactF (Map f (hembed p))

instance Functor (Rec (PatternF t) v)
  where fmap = (hembed .) . Map

instance Applicative (Rec (PatternF t) v)
  where pure = hembed . Ret . pure
        (<*>) = ((fmap (uncurry ($)) . hembed) .) . Cat

instance Alternative (Rec (PatternF t) v)
  where empty = hembed Nul
        (<|>) = (hembed .) . Alt
        some v = (:) <$> v <*> many v
        many = hembed . Rep

instance Monad (Rec (PatternF t) v)
  where return = pure
        (>>=) = (hembed .) . Bnd

instance Functor (Graph (PatternF t))
  where fmap f (Graph rec) = Graph (f <$> rec)

instance Applicative (Graph (PatternF t))
  where pure a = Graph (pure a)
        Graph f <*> Graph a = Graph (f <*> a)

instance Alternative (Graph (PatternF t))
  where empty = Graph empty
        Graph a <|> Graph b = Graph (a <|> b)
        some (Graph p) = Graph (some p)
        many (Graph p) = Graph (many p)

instance Monad (Graph (PatternF t))
  where return = pure
        Graph p >>= f = Graph (p >>= unGraph . f)

instance Functor (Fix (PatternF t))
  where fmap = (hembed .) . Map

instance Applicative (Fix (PatternF t))
  where pure = hembed . Ret . pure
        (<*>) = ((fmap (uncurry ($)) . hembed) .) . Cat

instance Alternative (Fix (PatternF t))
  where empty = hembed Nul
        (<|>) = (hembed .) . Alt
        some v = (:) <$> v <*> many v
        many = hembed . Rep

instance Monad (Fix (PatternF t))
  where return = pure
        (>>=) = (hembed .) . Bnd

instance HCorecursive (Rec (PatternF t) v)
  where hembed = liftRec compactF . wrap

instance HCorecursive (Fix (PatternF t))
  where hembed = Fix . compactF

instance Pattern (Rec (PatternF t) v) t
  where pattern (Rec (In r)) = Just r
        pattern _ = Nothing

instance Pattern (Fix (PatternF t)) t
  where pattern = Just . unFix
