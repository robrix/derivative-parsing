{-# LANGUAGE DeriveFunctor, FlexibleInstances, GADTs, MultiParamTypeClasses, RankNTypes #-}
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
, ParserF(..)
, Parser
, Combinator
, ret
, sep
, sep1
, size
, Derivative.Parser.mu
, parser
, combinator
, nullable
, isTerminal
) where

import Control.Applicative
import Data.Bifunctor (first)
import Data.Char
import Data.Higher.Foldable
import Data.Higher.Functor
import Data.Higher.Functor.Eq
import Data.Higher.Functor.Show
import Data.Higher.Graph as Graph
import qualified Data.Monoid as Monoid
import Data.Monoid hiding (Alt)

-- API

parse :: Parser a -> String -> [a]
parse p = parseNull . foldl deriv (compact p)


commaSep1 :: Combinator v a -> Combinator v [a]
commaSep1 = sep1 (char ',')

commaSep :: Combinator v a -> Combinator v [a]
commaSep = sep (char ',')

sep1 :: Combinator v sep -> Combinator v a -> Combinator v [a]
sep1 s p = (:) <$> p <*> many (s *> p)

sep :: Combinator v sep -> Combinator v a -> Combinator v [a]
sep s p = s `sep1` p <|> pure []

oneOf :: (Foldable t, Alternative f) => t (f a) -> f a
oneOf = getAlt . foldMap Monoid.Alt

infixl 4 `cat`

cat :: Combinator v a -> Combinator v b -> Combinator v (a, b)
cat a = compact' . rec . Cat a

char :: Char -> Combinator v Char
char = rec . Chr

category :: GeneralCategory -> Combinator v Char
category = rec . Uni

delta :: Combinator v a -> Combinator v a
delta = compact' . rec . Del

ret :: [a] -> Combinator v a
ret = rec . Ret

infixr 2 `label`

label :: Combinator v a -> String -> Combinator v a
label p = compact' . rec . Lab p

string :: String -> Combinator v String
string string = sequenceA (rec . Chr <$> string)

mu :: (Combinator v a -> Combinator v a) -> Combinator v a
mu f = Graph.mu $ \ v -> case f (var v) of
  Rec (In r) -> r
  p -> p `Lab` ""

parser :: (forall v. Combinator v a) -> Parser a
parser r = compact $ Graph r

combinator :: Parser a -> Combinator v a
combinator = unGraph


-- Types

-- | A parser type encoding concatenation, alternation, repetition, &c. as first-order constructors.
data ParserF f a where
  Cat :: f a -> f b -> ParserF f (a, b)
  Alt :: f a -> f a -> ParserF f a
  Rep :: f a -> ParserF f [a]
  Map :: (a -> b) -> f a -> ParserF f b
  Bnd :: f a -> (a -> f b) -> ParserF f b
  Chr :: Char -> ParserF f Char
  Uni :: GeneralCategory -> ParserF f Char
  Ret :: [a] -> ParserF f a
  Nul :: ParserF f a
  Lab :: f a -> String -> ParserF f a
  Del :: f a -> ParserF f a

type Parser = Graph ParserF
type Combinator v = Rec ParserF v


-- Algorithm

deriv :: Parser a -> Char -> Parser a
deriv g c = Graph (deriv' (unGraph g))
  where deriv' :: Combinator (Combinator v) a -> Combinator v a
        deriv' rc = case rc of
          Var v -> v
          Rec (Mu g) -> deriv'' (g (pjoin (Graph.mu g)))
          Rec (In r) -> deriv'' r
        deriv'' :: ParserF (Combinator (Combinator v)) a -> Combinator v a
        deriv'' p = case p of
          Cat a b -> deriv' a `cat` pjoin b <|> delta (pjoin a) `cat` deriv' b
          Alt a b -> deriv' a <|> deriv' b
          Rep a -> uncurry (:) <$> (deriv' a `cat` pjoin (many a))
          Map f p -> f <$> deriv' p
          Bnd p f -> deriv' p >>= pjoin . f
          Chr c' -> if c == c' then pure c else empty
          Uni category -> if generalCategory c == category then pure c else empty
          Lab p s -> deriv' p `label` s
          _ -> empty

parseNull :: Parser a -> [a]
parseNull = (`fold` []) $ \ parser -> case parser of
  Cat a b -> (,) <$> a <*> b
  Alt a b -> a <> b
  Rep a -> [[]]
  Map f p -> f <$> p
  Bnd p f -> p >>= f
  Ret as -> as
  Lab p _ -> p
  Del a -> a
  _ -> []

compact :: Parser a -> Parser a
compact = transform compact''

compact' :: Combinator v a -> Combinator v a
compact' = liftRec compact''

compact'' :: ParserF (Combinator v) a -> ParserF (Combinator v) a
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

nullable :: Parser a -> Bool
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

isTerminal :: Parser a -> Bool
isTerminal = (getConst .) $ (`fold` Const False) (Const . isTerminal'')

isTerminal'' :: ParserF f a -> Bool
isTerminal'' p = case p of
  Cat _ _ -> False
  Alt _ _ -> False
  Rep _ -> False
  Map _ _ -> False
  Bnd _ _ -> False
  Lab _ _ -> False
  _ -> True

size :: Parser a -> Int
size = getSum . getK . fold ((K (Sum 1) <|>) . hfoldMap id) (K (Sum 0))


-- Implementation details

newtype K a b = K { getK :: a }
  deriving (Eq, Functor, Ord, Show)


-- Instances

instance HFunctor ParserF where
  hfmap f p = case p of
    Cat a b -> Cat (f a) (f b)
    Alt a b -> Alt (f a) (f b)
    Rep a -> Rep (f a)
    Map g p -> Map g (f p)
    Bnd p g -> Bnd (f p) (f . g)
    Chr c -> Chr c
    Uni c -> Uni c
    Ret as -> Ret as
    Nul -> Nul
    Lab p s -> Lab (f p) s
    Del a -> Del (f a)

instance (Alternative a, Monad a) => HFoldable ParserF a where
  hfoldMap f p = case p of
    Cat a b -> f ((,) <$> a <*> b)
    Alt a b -> f (a <|> b)
    Map g p -> f (g <$> p)
    Bnd p g -> f (p >>= g)
    Lab p _ -> f p
    Del a -> f a
    _ -> empty

instance Functor (Rec ParserF v) where
  fmap f = compact' . rec . Map f

instance Functor (Graph ParserF) where
  fmap f (Graph rec) = Graph (f <$> rec)

instance Applicative (Rec ParserF v) where
  pure = rec . Ret . pure
  a <*> b = compact' (uncurry ($) <$> (a `cat` b))

instance Applicative (Graph ParserF) where
  pure a = Graph (pure a)
  Graph f <*> Graph a = Graph (f <*> a)

instance Alternative (Rec ParserF v) where
  empty = rec Nul
  a <|> b = compact' (rec (Alt a b))
  some v = (:) <$> v <*> many v
  many = compact' . rec . Rep

instance Alternative (Graph ParserF) where
  empty = Graph empty
  Graph a <|> Graph b = Graph (a <|> b)
  some (Graph p) = Graph (some p)
  many (Graph p) = Graph (many p)

instance Monad (Rec ParserF v) where
  return = pure
  (>>=) = (compact' .) . (rec .) . Bnd

instance Monad (Graph ParserF) where
  return = pure
  Graph p >>= f = Graph (p >>= unGraph . f)

instance HEqF ParserF
  where heqF eq a b = case (a, b) of
          (Cat a1 b1, Cat a2 b2) -> eq a1 a2 && eq b1 b2
          (Alt a1 b1, Alt a2 b2) -> eq a1 a2 && eq b1 b2
          -- (Map f1 p1, Map f2 p2) -> eq p1 p2
          -- (Bnd p1 f1, Bnd p2 f2) -> eq p1 p2
          (Chr c1, Chr c2) -> c1 == c2
          (Uni c1, Uni c2) -> c1 == c2
          (Ret r1, Ret r2) -> length r1 == length r2
          (Nul, Nul) -> True
          (Lab p1 s1, Lab p2 s2) -> s1 == s2 && eq p1 p2
          _ -> False

instance HShowF ParserF
  where hshowsPrecF showsPrec n p = case p of
          Cat a b -> showParen (n > 4) $ showsPrec 4 a . showString " `cat` " . showsPrec 5 b
          Alt a b -> showParen (n > 3) $ showsPrec 3 a . showString " <|> " . showsPrec 4 b
          Rep a -> showParen (n >= 10) $ showString "many " . showsPrec 10 a
          Map _ p -> showParen (n > 4) $ showString "f <$> " . showsPrec 5 p
          Bnd p _ -> showParen (n > 1) $ showsPrec 1 p . showString " >>= f"
          Chr c -> showParen (n >= 10) $ showString "char " . shows c
          Uni c -> showParen (n >= 10) $ showString "category " . shows c
          Ret [_] -> showParen (n >= 10) $ showString "pure t"
          Ret t -> showString "ret [" . showIndices (length t) . showString "]"
          Nul -> showString "empty"
          Lab p s -> showParen (n > 2) $ showsPrec 3 p . showString " `label` " . shows s
          Del a -> showParen (n >= 10) $ showString "delta " . showsPrec 10 a
          where showIndices n = foldr (.) id ((showChar 't' .) . shows <$> take n (iterate succ (0 :: Integer)))

instance Monoid a => Applicative (K a)
  where pure = const (K mempty)
        K a <*> K b = K (a <> b)

instance Monoid a => Alternative (K a)
  where empty = K mempty
        K a <|> K b = K (a <> b)

instance Monoid a => Monad (K a)
  where return = pure
        K a >>= _ = K a
