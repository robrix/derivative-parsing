{-# LANGUAGE GADTs #-}
module Derivative.Parser where

import Control.Applicative

-- API

literal :: String -> Parser String
literal string = sequenceA (Lit <$> string)

sep1 :: Parser sep -> Parser a -> Parser [a]
sep1 s p = (:) <$> p <*> many (s *> p)


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
