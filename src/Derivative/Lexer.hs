{-# LANGUAGE FlexibleInstances, GADTs #-}
module Derivative.Lexer
( Lexer
) where

import Control.Applicative
import Data.Higher.Functor.Recursive
import Data.Pattern

-- Types

type Lexer t v = Fix (PatternF t) v


-- Instances

instance Functor (Fix (PatternF t) v) where
  fmap f = Fix . Map f

instance Applicative (Fix (PatternF t) v) where
  pure = Fix . Ret . pure
  a <*> b = uncurry ($) <$> Fix (Cat a b)

instance Alternative (Fix (PatternF t) v) where
  empty = Fix Nul
  a <|> b = Fix (Alt a b)
  some v = (:) <$> v <*> many v
  many = Fix . Rep

instance Monad (Fix (PatternF t) v) where
  return = pure
  (>>=) = (Fix .) . Bnd
