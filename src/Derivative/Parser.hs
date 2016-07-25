{-# LANGUAGE DeriveFunctor, FlexibleInstances, GADTs, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, TypeSynonymInstances #-}
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
import Data.Higher.Functor.Recursive
import qualified Data.Higher.Graph as Graph
import Data.Higher.Graph hiding (mu, fold, transform, liftRec, pjoin, Rec(..))
import qualified Data.Monoid as Monoid
import Data.Monoid hiding (Alt)
import Data.Pattern
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
cat a = hembed . Cat a

char :: Char -> Combinator Char v Char
char = hembed . Sat . Equal

category :: GeneralCategory -> Combinator Char v Char
category = hembed . Sat . Category

delta :: Combinator t v a -> Combinator t v a
delta = hembed . Del

ret :: [a] -> Combinator t v a
ret = hembed . Ret

infixr 2 `label`

label :: Combinator t v a -> String -> Combinator t v a
label p = hembed . Lab p

string :: String -> Combinator Char v String
string string = sequenceA (hembed . Sat . Equal <$> string)

mu :: (Combinator t v a -> Combinator t v a) -> Combinator t v a
mu f = Rec $ Mu $ \ v -> case f (Var v) of
  Rec (In r) -> r
  p -> p `Lab` ""

anyToken :: Combinator t v t
anyToken = hembed (Sat (Constant True))


parser :: (forall v. Combinator t v a) -> Parser t a
parser r = compact $ Parser r


-- Types

newtype Parser t a = Parser { combinator :: forall v. Combinator t v a }
data Rec f v a = Var (v a) | Rec (RecF f v (Rec f v) a)
type Combinator t = Rec (PatternF t)


-- Algorithm

deriv :: forall a t. Parser t a -> t -> Parser t a
deriv g c = Parser (deriv' (combinator g))
  where deriv' :: forall a v. Combinator t (Graph.Rec (PatternF t) v) a -> Combinator t v a
        deriv' rc = case rc of
          Var v -> fromRec v
          Rec (Mu g) -> deriv'' (g (Graph.pjoin (Graph.mu (hfmap toRec . g))))
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
compact = transform compact''

compact' :: Combinator v t a -> Combinator v t a
compact' = liftRec compact''

compact'' :: PatternF t (Combinator t v) a -> PatternF t (Combinator t v) a
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


fold :: (forall a. PatternF t c a -> c a) -> (forall a. c a) -> Parser t a -> c a
fold f z = Graph.fold f z . toGraph

transform :: (forall a v. PatternF t (Combinator t v) a -> PatternF t (Combinator t v) a) -> Parser t a -> Parser t a
transform f = fromGraph . Graph.transform (hfmap toRec . f . hfmap fromRec) . toGraph

liftRec :: (forall a. PatternF t (Combinator t v) a -> PatternF t (Combinator t v) a) -> Combinator t v a -> Combinator t v a
liftRec f = fromRec . Graph.liftRec (hfmap toRec . f . hfmap fromRec) . toRec

pjoin :: Combinator t (Graph.Rec (PatternF t) v) a -> Combinator t v a
pjoin = fromRec . Graph.pjoin . toRec

toGraph :: Parser t a -> Graph (PatternF t) a
toGraph (Parser r) = Graph (toRec r)

fromGraph :: Graph (PatternF t) a -> Parser t a
fromGraph (Graph r) = Parser (fromRec r)

fromRec :: Graph.Rec (PatternF t) v a -> Combinator t v a
fromRec (Graph.Var v) = Var v
fromRec (Graph.Rec r) = Rec (hfmap fromRec r)

toRec :: Combinator t v a -> Graph.Rec (PatternF t) v a
toRec (Var v) = Graph.Var v
toRec (Rec r) = Graph.Rec (hfmap toRec r)


-- Instances

instance Functor (Parser t) where
  fmap f (Parser rec) = Parser (f <$> rec)

instance Applicative (Parser t) where
  pure a = Parser (pure a)
  Parser f <*> Parser a = Parser (f <*> a)

instance Alternative (Parser t) where
  empty = Parser empty
  Parser a <|> Parser b = Parser (a <|> b)
  some (Parser p) = Parser (some p)
  many (Parser p) = Parser (many p)

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
  where a == b = eqRec 0 (toRec (combinator a)) (toRec (combinator b))

instance Show t => Show (Parser t a)
  where showsPrec n = showsPrec n . (toRec . combinator :: Parser t a -> Graph.Rec (PatternF t) (Const Char) a)

instance HCorecursive Rec (PatternF t) where hembed = compact' . fromRec . Graph.rec . hfmap toRec
