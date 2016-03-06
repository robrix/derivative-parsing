{-# LANGUAGE GADTs #-}
module Derivative.Parser where

data Parser a where
  Cat :: Parser a -> Parser b -> Parser (a, b)
  Alt :: Parser a -> Parser b -> Parser (Either a b)
  Rep :: Parser a -> Parser [a]
  Map :: Parser a -> (a -> b) -> Parser b
  App :: Parser (a -> b) -> Parser a -> Parser b
  Lit :: Char -> Parser Char
  Ret :: [a] -> Parser a
  Nul :: Parser a
  Eps :: Parser a
