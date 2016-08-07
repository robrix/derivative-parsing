module Derivative.Lexer.Spec where

import Control.Applicative
import Derivative.Lexer
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "parseNull" $ do
    describe "cat" $ do
      prop "returns pairs of its parse trees" $
        \ a b -> parseNull (pure a `cat` pure b) `shouldBe` [(a, b) :: (Char, Char)]

      prop "is empty when its left operand is empty" $
        \ b -> parseNull (empty `cat` pure b) `shouldBe` ([] :: [(Char, Char)])

      prop "is empty when its right operand is empty" $
        \ a -> parseNull (pure a `cat` empty) `shouldBe` ([] :: [(Char, Char)])

    describe "<|>" $ do
      prop "returns left parse trees" $
        \ a -> parseNull (pure a <|> empty) `shouldBe` [a :: Char]

      prop "returns right parse trees" $
        \ b -> parseNull (empty <|> pure b) `shouldBe` [b :: Char]

      prop "returns ambiguous parse trees" $
        \ a b -> parseNull (pure a <|> pure b) `shouldBe` [a, b :: Char]

data LamT = Lambda | Dot | Identifier String

lexer :: Lexer Char LamT
lexer
    = (Lambda <$ char '\\' `label` "lambda")
  <|> (Dot <$ char '.' `label` "dot")
  <|> (Identifier <$> string "x" `label` "identifier")
