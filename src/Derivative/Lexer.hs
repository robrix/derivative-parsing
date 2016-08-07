{-# LANGUAGE FlexibleInstances, GADTs #-}
module Derivative.Lexer
( Lexer
) where

import Data.Higher.Functor.Fix
import Data.Pattern

-- Types

type Lexer t = Fix (PatternF t)
