{-# LANGUAGE FlexibleInstances, GADTs #-}
module Derivative.Lexer
( Lexer
, parseNull
, compact
, size
, module Pattern
) where

import Control.Applicative
import Data.Functor.K
import Data.Higher.Foldable
import Data.Higher.Functor.Fix
import Data.Monoid hiding (Alt)
import Data.Pattern as Pattern

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


compact :: Lexer t a -> Lexer t a
compact = Fix . compactF . unFix


size :: Lexer t a -> Int
size = getSum . getK . cata ((K (Sum 1) <|>) . hfoldMap id)
