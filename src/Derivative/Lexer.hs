{-# LANGUAGE FlexibleInstances, GADTs #-}
module Derivative.Lexer
( Lexer
, cat
, char
, ret
, label
, string
, parseNull
, size
) where

import Control.Applicative
import Data.Functor.K
import Data.Higher.Foldable
import Data.Higher.Functor.Fix
import Data.Higher.Functor.Recursive
import Data.Monoid hiding (Alt)
import Data.Pattern
import Data.Predicate

-- API

cat :: Lexer t a -> Lexer t b -> Lexer t (a, b)
cat a = hembed . Cat a

char :: Char -> Lexer Char Char
char = hembed . Sat . Equal

ret :: [a] -> Lexer t a
ret = hembed . Ret

infixr 2 `label`

label :: Lexer t a -> String -> Lexer t a
label p = hembed . Lab p

string :: String -> Lexer Char String
string string = sequenceA (hembed . Sat . Equal <$> string)


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

size :: Lexer t a -> Int
size = getSum . getK . cata ((K (Sum 1) <|>) . hfoldMap id)
