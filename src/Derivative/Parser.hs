{-# LANGUAGE GADTs #-}
module Derivative.Parser where

data Parser a where
  Lit :: Char -> Parser Char
  Ret :: [a] -> Parser a
  Nul :: Parser a
  Eps :: Parser a
