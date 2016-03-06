{-# LANGUAGE GADTs #-}
module Derivative.Parser where

import Control.Applicative

-- API

literal :: String -> Parser String
literal string = sequenceA (Lit <$> string)

commaSep1 :: Parser a -> Parser [a]
commaSep1 = sep1 (Lit ',')

commaSep :: Parser a -> Parser [a]
commaSep = sep (Lit ',')

sep1 :: Parser sep -> Parser a -> Parser [a]
sep1 s p = (:) <$> p <*> many (s *> p)

sep :: Parser sep -> Parser a -> Parser [a]
sep s p = s `sep1` p <|> pure []


-- Types

-- | A parser type encoding concatenation, alternation, repetition, &c. as first-order constructors.
data Parser a where
  Cat :: Parser a -> Parser b -> Parser (a, b)
  Alt :: Parser a -> Parser b -> Parser (Either a b)
  Rep :: Parser a -> Parser [a]
  Map :: (a -> b) -> Parser a -> Parser b
  App :: Parser (a -> b) -> Parser a -> Parser b
  Bnd :: Parser a -> (a -> Parser b) -> Parser b
  Lit :: Char -> Parser Char
  Ret :: [a] -> Parser a
  Nul :: Parser a
  Eps :: Parser a


-- Algorithm

parseNull :: Parser a -> [a]
parseNull (Rep _) = [[]]
parseNull (Ret as) = as
parseNull _ = []

compact :: Parser a -> Parser a
compact (Cat Nul _) = Nul
compact (Cat _ Nul) = Nul
compact (Cat (Ret [t]) b) = (,) t <$> b
compact (Cat a (Ret [t])) = flip (,) t <$> a
compact (Alt Nul p) = Right <$> p
compact (Alt p Nul) = Left <$> p
compact (Map f (Ret as)) = Ret (f <$> as)
compact (Map g (Map f p)) = g . f <$> p
compact (Rep Nul) = Ret []
compact a = a


-- Instances

instance Functor Parser where
  fmap = Map

instance Applicative Parser where
  pure = Ret . pure
  (<*>) = App

instance Alternative Parser where
  empty = Eps
  (<|>) = (fmap (either id id) .) . Alt
  some v = (:) <$> v <*> many v
  many = Rep

instance Monad Parser where
  return = Ret . pure
  (>>=) = Bnd
