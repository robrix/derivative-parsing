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
, Derivative.Parser.isTerminal
) where

import Control.Applicative
import Data.Char
import Data.Foldable (foldl')
import Data.Higher.Foldable
import Data.Higher.Graph as Graph hiding (wrap)
import qualified Data.Monoid as Monoid
import Data.Monoid hiding (Alt)
import Data.Pattern as Pattern
import Data.Predicate

-- API

parse :: Foldable f => Parser t a -> f t -> [a]
parse p = parseNull . foldl' deriv (compact p)


commaSep1 :: Combinator Char v a -> Combinator Char v [a]
commaSep1 = sep1 (char ',')

commaSep :: Combinator Char v a -> Combinator Char v [a]
commaSep = sep (char ',')

sep1 :: Combinator t v sep -> Combinator t v a -> Combinator t v [a]
sep1 s p = (:) <$> p <*> many (s *> p)

sep :: Combinator t v sep -> Combinator t v a -> Combinator t v [a]
sep s p = s `sep1` p <|> pure []

oneOf :: (Foldable t, Alternative f) => t (f a) -> f a
oneOf = getAlt . foldMap Monoid.Alt

infixl 4 `cat`

cat :: Combinator t v a -> Combinator t v b -> Combinator t v (a, b)
cat a = wrap . Cat a

char :: Char -> Combinator Char v Char
char = wrap . Sat . Equal

category :: GeneralCategory -> Combinator Char v Char
category = wrap . Sat . Category

delta :: Combinator t v a -> Combinator t v a
delta = wrap . Del

ret :: [a] -> Combinator t v a
ret = wrap . Ret

infixr 2 `label`

label :: Combinator t v a -> String -> Combinator t v a
label p = wrap . Lab p

string :: String -> Combinator Char v String
string string = sequenceA (wrap . Sat . Equal <$> string)

mu :: (Combinator t v a -> Combinator t v a) -> Combinator t v a
mu f = Rec $ Mu $ \ v -> case f (Var v) of
  Rec (In r) -> r
  p -> p `Lab` ""

anyToken :: Combinator t v t
anyToken = wrap (Sat (Constant True))


parser :: (forall v. Combinator t v a) -> Parser t a
parser r = compact $ Graph r

combinator :: Parser t a -> Combinator t v a
combinator = unGraph


-- Types

type Parser t = Graph (PatternF t)
type Combinator t = Rec (PatternF t)


-- Algorithm

deriv :: forall a t. Parser t a -> t -> Parser t a
deriv g c = Graph (deriv' (combinator g))
  where deriv' :: forall a v. Combinator t (Graph.Rec (PatternF t) v) a -> Combinator t v a
        deriv' rc = case rc of
          Var v -> v
          Rec (Mu g) -> deriv'' (g (Graph.pjoin (Graph.mu g)))
          Rec (In r) -> deriv'' r
        deriv'' :: forall a v. PatternF t (Combinator t (Graph.Rec (PatternF t) v)) a -> Combinator t v a
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
compact = transform compactF

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
isTerminal = (getConst .) $ (`fold` Const False) (Const . Pattern.isTerminal)


size :: Parser t a -> Int
size = getSum . getK . fold ((K (Sum 1) <|>) . hfoldMap id) (K (Sum 0))


-- Implementation details

newtype K a b = K { getK :: a }
  deriving (Eq, Functor, Ord, Show)


-- Instances

instance Monoid a => Applicative (K a)
  where pure = const (K mempty)
        K a <*> K b = K (a <> b)

instance Monoid a => Alternative (K a)
  where empty = K mempty
        K a <|> K b = K (a <> b)

instance Monoid a => Monad (K a)
  where return = pure
        K a >>= _ = K a
