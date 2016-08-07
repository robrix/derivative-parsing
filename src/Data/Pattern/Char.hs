{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
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

space :: (Alternative r, Pattern r Char) => r Char
space = oneOf (category <$> [Space .. ParagraphSeparator]) <|> oneOf (char <$> "\t\n\r\f\v")

upper :: Pattern r Char => r Char
upper = category UppercaseLetter

lower :: Pattern r Char => r Char
lower = category LowercaseLetter

alphaNum :: (Alternative r, Pattern r Char) => r Char
alphaNum = letter <|> oneOf (category <$> [DecimalNumber .. OtherNumber])

letter :: (Alternative r, Pattern r Char) => r Char
letter = oneOf (category <$> [UppercaseLetter .. OtherLetter])

digit :: (Alternative r, Pattern r Char) => r Char
digit = oneOf (char <$> ['0'..'9'])

octDigit :: (Alternative r, Pattern r Char) => r Char
octDigit = oneOf (char <$> ['0'..'7'])

hexDigit :: (Alternative r, Pattern r Char) => r Char
hexDigit = digit <|> oneOf (char <$> ['a'..'f']) <|> oneOf (char <$> ['A'..'F'])

newline :: Pattern r Char => r Char
newline = char '\n'

crlf :: (Applicative r, Pattern r Char) => r Char
crlf = char '\r' *> newline

endOfLine :: (Alternative r, Pattern r Char) => r Char
endOfLine = newline <|> crlf
