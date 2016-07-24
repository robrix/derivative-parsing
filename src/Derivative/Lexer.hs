{-# LANGUAGE GADTs #-}
module Derivative.Lexer
( Lexer
) where

import Data.Higher.Fix
import Data.Pattern

-- Types

type Lexer t = Fix (PatternF t)


-- Instances
