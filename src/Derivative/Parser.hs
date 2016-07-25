{-# LANGUAGE DeriveFunctor, FlexibleInstances, GADTs, RankNTypes, ScopedTypeVariables, TypeSynonymInstances #-}
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
import qualified Data.Higher.Graph as Graph
import Data.Higher.Graph hiding (rec, mu, fold, transform)
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
cat a = rec . Cat a

char :: Char -> Combinator v Char Char
char = rec . Sat . Equal

category :: GeneralCategory -> Combinator v Char Char
category = rec . Sat . Category

delta :: Combinator v t a -> Combinator v t a
delta = rec . Del

ret :: [a] -> Combinator v t a
ret = rec . Ret

infixr 2 `label`

label :: Combinator v t a -> String -> Combinator v t a
label p = rec . Lab p

string :: String -> Combinator v Char String
string string = sequenceA (rec . Sat . Equal <$> string)

mu :: (Combinator v t a -> Combinator v t a) -> Combinator v t a
mu f = Graph.mu $ \ v -> case f (var v) of
  Rec (In r) -> r
  p -> p `Lab` ""

anyToken :: Combinator v t t
anyToken = rec (Sat (Constant True))


parser :: (forall v. Combinator v t a) -> Parser t a
parser r = compact $ Parser r


-- Types

newtype Parser t a = Parser { combinator :: forall v. Combinator v t a }
type Combinator v t = Rec (PatternF t) v


-- Algorithm

deriv :: forall a t. Parser t a -> t -> Parser t a
deriv g c = Parser (deriv' (combinator g))
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


rec :: PatternF t (Combinator v t) a -> Combinator v t a
rec = compact' . Graph.rec

fold :: (forall a. PatternF t c a -> c a) -> (forall a. c a) -> Parser t a -> c a
fold f z = Graph.fold f z . toGraph

transform :: (forall a v. PatternF t (Rec (PatternF t) v) a -> PatternF t (Rec (PatternF t) v) a) -> Parser t a -> Parser t a
transform f = fromGraph . Graph.transform f . toGraph

toGraph :: Parser t a -> Graph (PatternF t) a
toGraph (Parser r) = Graph r

fromGraph :: Graph (PatternF t) a -> Parser t a
fromGraph (Graph r) = Parser r


-- Instances

instance Functor (Combinator v t) where
  fmap f = rec . Map f

instance Functor (Parser t) where
  fmap f (Parser rec) = Parser (f <$> rec)

instance Applicative (Combinator v t) where
  pure = rec . Ret . pure
  a <*> b = uncurry ($) <$> (a `cat` b)

instance Applicative (Parser t) where
  pure a = Parser (pure a)
  Parser f <*> Parser a = Parser (f <*> a)

instance Alternative (Combinator v t) where
  empty = rec Nul
  a <|> b = rec (Alt a b)
  some v = (:) <$> v <*> many v
  many = rec . Rep

instance Alternative (Parser t) where
  empty = Parser empty
  Parser a <|> Parser b = Parser (a <|> b)
  some (Parser p) = Parser (some p)
  many (Parser p) = Parser (many p)

instance Monad (Combinator v t) where
  return = pure
  (>>=) = (rec .) . Bnd

instance Monad (Parser t) where
  return = pure
  Parser p >>= f = Parser (p >>= combinator . f)

instance Monoid a => Applicative (K a)
  where pure = const (K mempty)
        K a <*> K b = K (a <> b)

instance Monoid a => Alternative (K a)
  where empty = K mempty
        K a <|> K b = K (a <> b)

instance Monoid a => Monad (K a)
  where return = pure
        K a >>= _ = K a

instance Eq (Parser t a)
  where a == b = eqRec 0 (combinator a) (combinator b)

instance Show t => Show (Parser t a)
  where showsPrec n = showsPrec n . (combinator :: Parser t a -> Rec (PatternF t) (Const Char) a)
