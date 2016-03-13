{-# LANGUAGE FlexibleInstances, GADTs, GeneralizedNewtypeDeriving, RankNTypes #-}
module Derivative.Parser
( alt
, cat
, commaSep
, commaSep1
, deriv
, eps
, lit
, literal
, nul
, oneOf
, parse
, parseNull
, Parser()
, ret
, sep
, sep1
, size
) where

import Control.Applicative
import Data.Monoid hiding (Alt)

-- API

parse :: Parser a -> String -> [a]
parse p = parseNull . foldl ((compact .) . deriv) p


cat :: Parser a -> Parser b -> Parser (a, b)
Parser a `cat` Parser b = Parser . F $ Cat a b

alt :: Parser a -> Parser b -> Parser (Either a b)
Parser a `alt` Parser b = Parser . F $ Alt a b

lit :: Char -> Parser Char
lit = Parser . F . Lit

ret :: [a] -> Parser a
ret = Parser . F . Ret

nul :: Parser a
nul = Parser $ F Nul

eps :: Parser a
eps = Parser $ F Eps

literal :: String -> Parser String
literal string = sequenceA (Parser . F . Lit <$> string)

commaSep1 :: Parser a -> Parser [a]
commaSep1 = sep1 (Parser (F (Lit ',')))

commaSep :: Parser a -> Parser [a]
commaSep = sep (Parser (F (Lit ',')))

sep1 :: Parser sep -> Parser a -> Parser [a]
sep1 s p = (:) <$> p <*> many (s *> p)

sep :: Parser sep -> Parser a -> Parser [a]
sep s p = s `sep1` p <|> pure []

oneOf :: (Foldable t, Alternative f) => t a -> f a
oneOf = foldr ((<|>) . pure) empty


-- Types

-- | A parser type encoding concatenation, alternation, repetition, &c. as first-order constructors.
data ParserF f a where
  Cat :: f a -> f b -> ParserF f (a, b)
  Alt :: f a -> f b -> ParserF f (Either a b)
  Rep :: f a -> ParserF f [a]
  Map :: (a -> b) -> f a -> ParserF f b
  Bnd :: f a -> (a -> f b) -> ParserF f b
  Lit :: Char -> ParserF f Char
  Ret :: [a] -> ParserF f a
  Nul :: ParserF f a
  Eps :: ParserF f a

newtype Parser a = Parser { unParser :: HFix ParserF a }
  deriving (Alternative, Applicative, Functor, Monad)


-- Algorithm

deriv :: Parser a -> Char -> Parser a
deriv (Parser f) c = Parser (deriv' f c)

deriv' :: HFix ParserF a -> Char -> HFix ParserF a
deriv' (F parser) c = case parser of
  Cat a b -> F (Cat (deriv' a c) b) <|> F (Cat (F (Ret (parseNull' a))) (deriv' b c))
  Alt a b -> F (Alt (deriv' a c) (deriv' b c))
  Rep p -> (:) <$> deriv' p c <*> F (Rep p)
  Map f p -> F (Map f (deriv' p c))
  Bnd p f -> F (Bnd (deriv' p c) f)
  Lit c' -> F $ if c == c' then Ret [c] else Nul
  _ -> F Nul

parseNull :: Parser a -> [a]
parseNull = parseNull' . unParser

parseNull' :: HFix ParserF a -> [a]
parseNull' (F parser) = case parser of
  Cat a b -> (,) <$> parseNull' a <*> parseNull' b
  Alt a b -> (Left <$> parseNull' a) ++ (Right <$> parseNull' b)
  Rep _ -> [[]]
  Map f p -> f <$> parseNull' p
  Bnd p f -> (f <$> parseNull' p) >>= parseNull'
  Ret as -> as
  _ -> []

compact :: Parser a -> Parser a
compact (Parser (F parser)) = Parser $ case parser of
  Cat (F Nul) _ -> F Nul
  Cat _ (F Nul) -> F Nul
  Cat (F (Ret [t])) b -> (,) t <$> b
  Cat a (F (Ret [t])) -> flip (,) t <$> a
  Alt (F Nul) p -> Right <$> p
  Alt p (F Nul) -> Left <$> p
  Map f (F (Ret as)) -> F (Ret (f <$> as))
  Map g (F (Map f p)) -> (g . f <$> p)
  Rep (F Nul) -> F (Ret [])
  a -> F a

size :: Parser a -> Int
size (Parser parser) = getSum $ getConst $ hcata size parser
  where size :: ParserF (Const (Sum Int)) a -> Const (Sum Int) a
        size = Const . mappend (Sum 1) . hfoldMap getConst


-- Implementation details

-- See http://www.timphilipwilliams.com/posts/2013-01-16-fixing-gadts.html for details about the higher-order functionality implemented here.

newtype HFix f a = F { out :: f (HFix f) a }

hcata :: HFunctor h => (forall out. h f out -> f out) -> (forall out. HFix h out -> f out)
hcata algebra = algebra . hfmap (hcata algebra) . out

class HFunctor h where
  hfmap :: (forall a. f a -> g a) -> (forall a. h f a -> h g a)

class HFoldable h where
  hfoldMap :: Monoid m => (forall b. f b -> m) -> h f a -> m

-- Instances

instance HFunctor ParserF where
  hfmap f p = case p of
    Cat a b -> Cat (f a) (f b)
    Alt a b -> Alt (f a) (f b)
    Rep p -> Rep (f p)
    Map g p -> Map g (f p)
    Bnd p g -> Bnd (f p) (f . g)
    Lit c -> Lit c
    Ret as -> Ret as
    Nul -> Nul
    Eps -> Eps

instance HFoldable ParserF where
  hfoldMap f p = case p of
    Cat a b -> f a <> f b
    Alt a b -> f a <> f b
    Rep p -> f p
    Map _ p -> f p
    Bnd p _ -> f p
    _ -> mempty

instance Functor (ParserF (HFix ParserF)) where
  fmap = (. F) . Map

instance Functor (HFix ParserF) where
  fmap = (F .) . Map

instance Applicative (HFix ParserF) where
  pure = F . Ret . pure
  (<*>) = (fmap (uncurry ($)) .) . (F .) . Cat

instance Alternative (HFix ParserF) where
  empty = F Nul
  (<|>) = (fmap (either id id) .) . (F .) . Alt
  some v = (:) <$> v <*> many v
  many = F . Rep

instance Monad (HFix ParserF) where
  return = pure
  (>>=) = (F .) . Bnd
