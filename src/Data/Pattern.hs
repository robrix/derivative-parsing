{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses #-}
module Data.Pattern
( PatternF(..)
, commaSep1
, commaSep
, sep1
, sep
, oneOf
, cat
, char
, category
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
import Data.Predicate
import Data.Higher.Foldable
import Data.Higher.Functor
import Data.Higher.Functor.Eq
import Data.Higher.Functor.Fix
import Data.Higher.Functor.Recursive
import Data.Higher.Functor.Show
import Data.Higher.Graph
import qualified Data.Monoid as Monoid
import Data.Monoid hiding (Alt)

-- Types

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


-- Smart constructors

commaSep1 :: (Alternative r, HCorecursive r, Base r ~ PatternF Char) => r a -> r [a]
commaSep1 = sep1 (char ',')

commaSep :: (Alternative r, HCorecursive r, Base r ~ PatternF Char) => r a -> r [a]
commaSep = sep (char ',')

sep1 :: Alternative r => r sep -> r a -> r [a]
sep1 s p = (:) <$> p <*> many (s *> p)

sep :: Alternative r => r sep -> r a -> r [a]
sep s p = s `sep1` p <|> pure []

oneOf :: (Foldable t, Alternative f) => t (f a) -> f a
oneOf = getAlt . foldMap Monoid.Alt

infixl 4 `cat`

cat :: (HCorecursive r, Base r ~ PatternF t) => r a -> r b -> r (a, b)
cat a = hembed . Cat a

char :: (HCorecursive r, Base r ~ PatternF Char) => Char -> r Char
char = hembed . Sat . Equal

category :: (HCorecursive r, Base r ~ PatternF Char) => GeneralCategory -> r Char
category = hembed . Sat . Category

delta :: (HCorecursive r, Base r ~ PatternF t) => r a -> r a
delta = hembed . Del

ret :: (HCorecursive r, Base r ~ PatternF t) => [a] -> r a
ret = hembed . Ret

infixr 2 `label`

label :: (HCorecursive r, Base r ~ PatternF t) => r a -> String -> r a
label p = hembed . Lab p

string :: (Applicative r, HCorecursive r, Base r ~ PatternF Char) => String -> r String
string string = sequenceA (hembed . Sat . Equal <$> string)

anyToken :: (HCorecursive r, Base r ~ PatternF a) => r a
anyToken = hembed (Sat (Constant True))


-- API

compactF :: PatternF t (Rec (PatternF t) v) a -> PatternF t (Rec (PatternF t) v) a
compactF pattern = case pattern of
  Cat (Rec (In Nul)) _ -> Nul
  Cat _ (Rec (In Nul)) -> Nul
  Cat (Rec (In (Ret [t]))) b -> Map ((,) t) b
  Cat a (Rec (In (Ret [t]))) -> Map (flip (,) t) a
  Cat (Rec (In (Cat a b))) c -> Map (\ (a, (b, c)) -> ((a, b), c)) (hembed (Cat a (hembed (Cat b c))))
  Cat (Rec (In (Map f a))) b -> Map (first f) (hembed (Cat a b))
  Alt (Rec (In Nul)) (Rec (In p)) -> p
  Alt (Rec (In p)) (Rec (In Nul)) -> p
  Alt (Rec (In (Ret a))) (Rec (In (Ret b))) -> Ret (a <> b)
  Rep (Rec (In Nul)) -> Ret [[]]
  Map f (Rec (In (Ret as))) -> Ret (f <$> as)
  Map g (Rec (In (Map f p))) -> Map (g . f) p
  Map _ (Rec (In Nul)) -> Nul
  Lab (Rec (In Nul)) _ -> Nul
  Lab (Rec (In (Ret t))) _ -> Ret t
  Lab (Rec (In (Del p))) _ -> Del p
  Del (Rec (In Nul)) -> Nul
  Del (Rec (In (Del p))) -> Del p
  Del (Rec (In (Ret a))) -> Ret a
  Del (Rec (In p)) | isTerminal p -> Nul
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
  where fmap = (Fix .) . Map

instance Applicative (Fix (PatternF t))
  where pure = Fix . Ret . pure
        (<*>) = (((fmap (uncurry ($))) . Fix) .) . Cat

instance Alternative (Fix (PatternF t))
  where empty = Fix Nul
        (<|>) = (Fix .) . Alt

instance Monad (Fix (PatternF t))
  where return = pure
        (>>=) = (Fix .) . Bnd

instance HCorecursive (Rec (PatternF t) v)
  where hembed = liftRec compactF . wrap

instance HCorecursive (Fix (PatternF t))
  where hembed = Fix
