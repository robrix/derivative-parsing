{-# LANGUAGE FlexibleInstances, GADTs #-}
module Derivative.Lexer
( Lexer
) where

import Control.Applicative
import Data.Higher.Fix
import Data.Pattern

-- Types

type Lexer t = Fix (PatternF t)


-- Instances

instance Functor (Fix (PatternF t)) where
  fmap f = Fix . Map f

instance Applicative (Fix (PatternF t)) where
  pure = Fix . Ret . pure
  a <*> b = uncurry ($) <$> Fix (Cat a b)

instance Alternative (Fix (PatternF t)) where
  empty = Fix Nul
  a <|> b = Fix (Alt a b)
  some v = (:) <$> v <*> many v
  many = Fix . Rep

instance Monad (Fix (PatternF t)) where
  return = pure
  (>>=) = (Fix .) . Bnd
