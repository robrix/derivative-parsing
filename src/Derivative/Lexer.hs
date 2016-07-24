{-# LANGUAGE GADTs #-}
module Derivative.Lexer
( LexerF(..)
) where

data LexerF t f a where
  Cat :: f a -> f b -> LexerF t f (a, b)
  Alt :: f a -> f b -> LexerF t f (a, b)
  Rep :: f a -> LexerF t f [a]
  Map :: (a -> b) -> f a -> LexerF t f b
  Bnd :: f a -> (a -> f b) -> LexerF t f b
  Ret :: [a] -> LexerF t f a
  Nul :: LexerF t f a
  Lab :: f a -> String -> LexerF t f a
  Del :: f a -> LexerF t f a


-- Instances
