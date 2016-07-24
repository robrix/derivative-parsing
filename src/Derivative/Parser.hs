{-# LANGUAGE DeriveFunctor, FlexibleInstances, GADTs, RankNTypes, ScopedTypeVariables #-}
module Derivative.Parser
( cat
, commaSep
, commaSep1
, compact
, deriv
, label
, char
, category
, string
, oneOf
, parse
, parseNull
, PatternF(..)
, Parser
, Combinator
, ret
, sep
, sep1
, size
, Derivative.Parser.mu
, anyToken
, parser
, combinator
, nullable
, isTerminal
) where

import Control.Applicative
import Data.Bifunctor (first)
import Data.Char
import Data.Foldable (foldl')
import Data.Higher.Foldable
import Data.Higher.Functor
import Data.Higher.Functor.Eq
import Data.Higher.Functor.Show
import Data.Higher.Graph as Graph
import qualified Data.Monoid as Monoid
import Data.Monoid hiding (Alt)
import Data.Pattern
import Data.Predicate

-- API

parse :: Foldable f => Parser t a -> f t -> [a]
parse p = parseNull . foldl' deriv (compact p)


commaSep1 :: Combinator v Char a -> Combinator v Char [a]
commaSep1 = sep1 (char ',')

commaSep :: Combinator v Char a -> Combinator v Char [a]
commaSep = sep (char ',')

sep1 :: Combinator v t sep -> Combinator v t a -> Combinator v t [a]
sep1 s p = (:) <$> p <*> many (s *> p)

sep :: Combinator v t sep -> Combinator v t a -> Combinator v t [a]
sep s p = s `sep1` p <|> pure []

oneOf :: (Foldable t, Alternative f) => t (f a) -> f a
oneOf = getAlt . foldMap Monoid.Alt

infixl 4 `cat`

cat :: Combinator v t a -> Combinator v t b -> Combinator v t (a, b)
cat a = compact' . rec . Cat a

char :: Char -> Combinator v Char Char
char = rec . Sat . Equal

category :: GeneralCategory -> Combinator v Char Char
category = rec . Sat . Category

delta :: Combinator v t a -> Combinator v t a
delta = compact' . rec . Del

ret :: [a] -> Combinator v t a
ret = rec . Ret

infixr 2 `label`

label :: Combinator v t a -> String -> Combinator v t a
label p = compact' . rec . Lab p

string :: String -> Combinator v Char String
string string = sequenceA (rec . Sat . Equal <$> string)

mu :: (Combinator v t a -> Combinator v t a) -> Combinator v t a
mu f = Graph.mu $ \ v -> case f (var v) of
  Rec (In r) -> r
  p -> p `Lab` ""

anyToken :: Combinator v t t
anyToken = rec (Sat (Constant True))


parser :: (forall v. Combinator v t a) -> Parser t a
parser r = compact $ Graph r

combinator :: Parser t a -> Combinator v t a
combinator = unGraph


-- Types

type Parser t = Graph (PatternF t)
type Combinator v t = Rec (PatternF t) v


-- Algorithm

deriv :: forall a t. Parser t a -> t -> Parser t a
deriv g c = Graph (deriv' (unGraph g))
  where deriv' :: forall a v. Combinator (Combinator v t) t a -> Combinator v t a
        deriv' rc = case rc of
          Var v -> v
          Rec (Mu g) -> deriv'' (g (pjoin (Graph.mu g)))
          Rec (In r) -> deriv'' r
        deriv'' :: forall a v. PatternF t (Combinator (Combinator v t) t) a -> Combinator v t a
        deriv'' p = case p of
          Cat a b -> deriv' a `cat` pjoin b <|> delta (pjoin a) `cat` deriv' b
          Alt a b -> deriv' a <|> deriv' b
          Rep a -> uncurry (:) <$> (deriv' a `cat` pjoin (many a))
          Map f p -> f <$> deriv' p
          Bnd p f -> deriv' p >>= pjoin . f
          Sat p -> if c `satisfies` p then pure c else empty
          Lab p s -> deriv' p `label` s
          _ -> empty

parseNull :: Parser t a -> [a]
parseNull = (`fold` []) $ \ parser -> case parser of
  Cat a b -> (,) <$> a <*> b
  Alt a b -> a <> b
  Rep _ -> [[]]
  Map f p -> f <$> p
  Bnd p f -> p >>= f
  Ret as -> as
  Lab p _ -> p
  Del a -> a
  _ -> []

compact :: Parser t a -> Parser t a
compact = transform compact''

compact' :: Combinator v t a -> Combinator v t a
compact' = liftRec compact''

compact'' :: PatternF t (Combinator v t) a -> PatternF t (Combinator v t) a
compact'' parser = case parser of
  Cat (Rec (In Nul)) _ -> Nul
  Cat _ (Rec (In Nul)) -> Nul
  Cat (Rec (In (Ret [t]))) b -> Map ((,) t) b
  Cat a (Rec (In (Ret [t]))) -> Map (flip (,) t) a
  Cat (Rec (In (Cat a b))) c -> Map (\ (a, (b, c)) -> ((a, b), c)) (cat a (cat b c))
  Cat (Rec (In (Map f a))) b -> Map (first f) (cat a b)
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
  Del (Rec (In p)) | isTerminal'' p -> Nul
  a -> a

nullable :: Parser t a -> Bool
nullable = (getConst .) $ (`fold` Const False) $ \ p -> case p of
  Cat a b -> Const $ getConst a && getConst b
  Alt a b -> Const $ getConst a || getConst b
  Rep _ -> Const True
  Map _ p -> Const (getConst p)
  Bnd p _ -> Const (getConst p)
  Ret _ -> Const True
  Lab p _ -> p
  Del a -> a
  _ -> Const False

isTerminal :: Parser t a -> Bool
isTerminal = (getConst .) $ (`fold` Const False) (Const . isTerminal'')

isTerminal'' :: PatternF t f a -> Bool
isTerminal'' p = case p of
  Cat _ _ -> False
  Alt _ _ -> False
  Rep _ -> False
  Map _ _ -> False
  Bnd _ _ -> False
  Lab _ _ -> False
  _ -> True

size :: Parser t a -> Int
size = getSum . getK . fold ((K (Sum 1) <|>) . hfoldMap id) (K (Sum 0))


-- Implementation details

newtype K a b = K { getK :: a }
  deriving (Eq, Functor, Ord, Show)


-- Instances

instance Functor (Rec (PatternF t) v) where
  fmap f = compact' . rec . Map f

instance Functor (Graph (PatternF t)) where
  fmap f (Graph rec) = Graph (f <$> rec)

instance Applicative (Rec (PatternF t) v) where
  pure = rec . Ret . pure
  a <*> b = uncurry ($) <$> (a `cat` b)

instance Applicative (Graph (PatternF t)) where
  pure a = Graph (pure a)
  Graph f <*> Graph a = Graph (f <*> a)

instance Alternative (Rec (PatternF t) v) where
  empty = rec Nul
  a <|> b = compact' (rec (Alt a b))
  some v = (:) <$> v <*> many v
  many = compact' . rec . Rep

instance Alternative (Graph (PatternF t)) where
  empty = Graph empty
  Graph a <|> Graph b = Graph (a <|> b)
  some (Graph p) = Graph (some p)
  many (Graph p) = Graph (many p)

instance Monad (Rec (PatternF t) v) where
  return = pure
  (>>=) = (compact' .) . (rec .) . Bnd

instance Monad (Graph (PatternF t)) where
  return = pure
  Graph p >>= f = Graph (p >>= unGraph . f)

instance Monoid a => Applicative (K a)
  where pure = const (K mempty)
        K a <*> K b = K (a <> b)

instance Monoid a => Alternative (K a)
  where empty = K mempty
        K a <|> K b = K (a <> b)

instance Monoid a => Monad (K a)
  where return = pure
        K a >>= _ = K a
