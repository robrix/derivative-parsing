{-# LANGUAGE FlexibleInstances, GADTs #-}
module Derivative.Lexer
( Lexer
, cat
) where

import Data.Higher.Functor.Fix
import Data.Pattern

-- API

cat :: Lexer t a -> Lexer t b -> Lexer t (a, b)
cat a = Fix . Cat a


-- Types

type Lexer t = Fix (PatternF t)
