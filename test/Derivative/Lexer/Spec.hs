module Derivative.Lexer.Spec where

import Control.Applicative
import Data.Pattern.Char
import Derivative.Lexer
import Test.Hspec
import Test.Hspec.QuickCheck

{-# ANN module "HLint: ignore Functor law" #-}
{-# ANN module "HLint: ignore Monad law, right identity" #-}

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

  describe "size" $ do
    prop "is 1 for terminals" $
      \ a b -> let terminals = [ ret a, char b, empty, ret [] ] in sum (size <$> terminals) `shouldBe` length terminals

    prop "is 1 + the sum for unary nonterminals" $
      \ a s -> [ size (fmap id (char a)), size (char a >>= return), size (char a `label` s) ] `shouldBe` [ 2, 2, 2 ]

    prop "is 1 + the sum for binary nonterminals" $
      \ a b -> [ size (char a `cat` char b), size (char a <|> char b) ] `shouldBe` [ 3, 3 ]

    it "terminates on unlabelled acyclic grammars" $
      size (char 'c') `shouldBe` 1

    it "terminates on interesting lexers" $
      size lexer `shouldBe` 15

data LamT = Lambda | Dot | Identifier String

lexer :: Lexer Char LamT
lexer
    = (Lambda <$ char '\\' `label` "lambda")
  <|> (Dot <$ char '.' `label` "dot")
  <|> (Identifier <$> string "x" `label` "identifier")


data SexprT = OpenT | CloseT | AtomT String

sexprL :: Lexer Char [SexprT]
sexprL
    = many
    $ space
  *> ((AtomT .) . (:) <$> letter <*> many alphaNum
  <|> OpenT <$ char '('
  <|> CloseT <$ char ')')
  <* space
