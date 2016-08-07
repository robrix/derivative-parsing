{-# LANGUAGE TypeFamilies #-}
module Data.Pattern.Char
( space
, upper
, lower
, alphaNum
, letter
, digit
, octDigit
, hexDigit
, newline
, crlf
, endOfLine
) where

import Control.Applicative
import Data.Char
import Data.Higher.Functor.Recursive
import Derivative.Parser

space :: (Alternative r, HCorecursive r, Base r ~ PatternF Char) => r Char
space = oneOf (category <$> [Space .. ParagraphSeparator]) <|> oneOf (char <$> "\t\n\r\f\v")

upper :: (HCorecursive r, Base r ~ PatternF Char) => r Char
upper = category UppercaseLetter

lower :: (HCorecursive r, Base r ~ PatternF Char) => r Char
lower = category LowercaseLetter

alphaNum :: (Alternative r, HCorecursive r, Base r ~ PatternF Char) => r Char
alphaNum = letter <|> oneOf (category <$> [DecimalNumber .. OtherNumber])

letter :: (Alternative r, HCorecursive r, Base r ~ PatternF Char) => r Char
letter = oneOf (category <$> [UppercaseLetter .. OtherLetter])

digit :: (Alternative r, HCorecursive r, Base r ~ PatternF Char) => r Char
digit = oneOf (char <$> ['0'..'9'])

octDigit :: (Alternative r, HCorecursive r, Base r ~ PatternF Char) => r Char
octDigit = oneOf (char <$> ['0'..'7'])

hexDigit :: (Alternative r, HCorecursive r, Base r ~ PatternF Char) => r Char
hexDigit = digit <|> oneOf (char <$> ['a'..'f']) <|> oneOf (char <$> ['A'..'F'])

newline :: (HCorecursive r, Base r ~ PatternF Char) => r Char
newline = char '\n'

crlf :: (Applicative r, HCorecursive r, Base r ~ PatternF Char) => r Char
crlf = char '\r' *> newline

endOfLine :: (Alternative r, HCorecursive r, Base r ~ PatternF Char) => r Char
endOfLine = newline <|> crlf
