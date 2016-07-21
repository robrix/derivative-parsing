module Derivative.Parser.Char
( space
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
import Derivative.Parser

space :: Combinator v Char
space = oneOf (category <$> [Space .. ParagraphSeparator]) <|> oneOf (char <$> "\t\n\r\f\v")

alphaNum :: Combinator v Char
alphaNum = letter <|> oneOf (category <$> [DecimalNumber .. OtherNumber])

letter :: Combinator v Char
letter = oneOf (category <$> [UppercaseLetter .. OtherLetter])

digit :: Combinator v Char
digit = oneOf (char <$> ['0'..'9'])

octDigit :: Combinator v Char
octDigit = oneOf (char <$> ['0'..'7'])

hexDigit :: Combinator v Char
hexDigit = digit <|> oneOf (char <$> ['a'..'f']) <|> oneOf (char <$> ['A'..'F'])

newline :: Combinator v Char
newline = char '\n'

crlf :: Combinator v Char
crlf = char '\r' *> newline

endOfLine :: Combinator v Char
endOfLine = newline <|> crlf
