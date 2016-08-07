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

space :: Combinator Char v Char
space = oneOf (category <$> [Space .. ParagraphSeparator]) <|> oneOf (char <$> "\t\n\r\f\v")

upper :: Combinator Char v Char
upper = category UppercaseLetter

lower :: Combinator Char v Char
lower = category LowercaseLetter

alphaNum :: Combinator Char v Char
alphaNum = letter <|> oneOf (category <$> [DecimalNumber .. OtherNumber])

letter :: Combinator Char v Char
letter = oneOf (category <$> [UppercaseLetter .. OtherLetter])

digit :: Combinator Char v Char
digit = oneOf (char <$> ['0'..'9'])

octDigit :: Combinator Char v Char
octDigit = oneOf (char <$> ['0'..'7'])

hexDigit :: Combinator Char v Char
hexDigit = digit <|> oneOf (char <$> ['a'..'f']) <|> oneOf (char <$> ['A'..'F'])

newline :: Combinator Char v Char
newline = char '\n'

crlf :: Combinator Char v Char
crlf = char '\r' *> newline

endOfLine :: Combinator Char v Char
endOfLine = newline <|> crlf
