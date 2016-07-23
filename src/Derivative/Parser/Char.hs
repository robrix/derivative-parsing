module Derivative.Parser.Char
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
import Derivative.Parser

space :: Combinator v Char Char
space = oneOf (category <$> [Space .. ParagraphSeparator]) <|> oneOf (char <$> "\t\n\r\f\v")

upper :: Combinator v Char Char
upper = category UppercaseLetter

lower :: Combinator v Char Char
lower = category LowercaseLetter

alphaNum :: Combinator v Char Char
alphaNum = letter <|> oneOf (category <$> [DecimalNumber .. OtherNumber])

letter :: Combinator v Char Char
letter = oneOf (category <$> [UppercaseLetter .. OtherLetter])

digit :: Combinator v Char Char
digit = oneOf (char <$> ['0'..'9'])

octDigit :: Combinator v Char Char
octDigit = oneOf (char <$> ['0'..'7'])

hexDigit :: Combinator v Char Char
hexDigit = digit <|> oneOf (char <$> ['a'..'f']) <|> oneOf (char <$> ['A'..'F'])

newline :: Combinator v Char Char
newline = char '\n'

crlf :: Combinator v Char Char
crlf = char '\r' *> newline

endOfLine :: Combinator v Char Char
endOfLine = newline <|> crlf
