{-# LANGUAGE FlexibleInstances, GADTs #-}
module Derivative.Lexer
( Lexer
) where

import Data.Higher.Functor.Recursive
import Data.Pattern

-- Types

type Lexer t v = Fix (PatternF t) v
