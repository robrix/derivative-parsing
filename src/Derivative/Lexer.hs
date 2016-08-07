{-# LANGUAGE FlexibleInstances, GADTs #-}
module Derivative.Lexer
( Lexer
, cat
, char
, ret
, parseNull
) where

import Data.Higher.Functor.Fix
import Data.Monoid hiding (Alt)
import Data.Pattern
import Data.Predicate

-- API

cat :: Lexer t a -> Lexer t b -> Lexer t (a, b)
cat a = Fix . Cat a

char :: Char -> Lexer Char Char
char = Fix . Sat . Equal

ret :: [a] -> Lexer t a
ret = Fix . Ret


-- Types

type Lexer t = Fix (PatternF t)


-- Algorithm

parseNull :: Lexer t a -> [a]
parseNull = cata $ \ parser -> case parser of
  Cat a b -> (,) <$> a <*> b
  Alt a b -> a <> b
  Rep _ -> [[]]
  Map f p -> f <$> p
  Bnd p f -> p >>= f
  Ret as -> as
  Lab p _ -> p
  Del a -> a
  _ -> []
