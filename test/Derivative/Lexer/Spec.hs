module Derivative.Lexer.Spec where

import Control.Applicative
import Data.Pattern.Char
import Derivative.Lexer
import Prelude hiding (lex)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (label)

{-# ANN module "HLint: ignore Redundant do" #-}
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

  describe "deriv" $ do
    describe "many" $ do
      prop "produces a list of successful parses" $
        \ c -> parseNull (many (char c) `deriv` c) `shouldBe` [[c]]

      prop "produces a list of multiple successful parses" $
        \ c -> parseNull ((many (char c) `deriv` c) `deriv` c) `shouldBe` [[c, c]]

      prop "produces no parse trees when unsuccessful" $
        \ c -> parseNull (many (char c) `deriv` succ c) `shouldBe` []

      prop "is constant in lexer size" $
        \ c i -> let p = many (char c) in
          take i (size <$> iterate (`deriv` c) p) `shouldBe` take i (size p : repeat (succ (size p)))

    describe "fmap" $ do
      prop "distributivity" $
        \ f c -> parseNull (fmap (getBlind f :: Char -> Char) (pure c)) `shouldBe` [getBlind f c]

    describe "char" $ do
      prop "represents unmatched content with the empty lexer" $
        \ a -> char a `deriv` succ a `shouldBe` empty

      prop "represents matched content with ε reduction lexers" $
        \ a -> char a `deriv` a `shouldBe` ret [a]

    describe "<|>" $ do
      prop "distributivity" $
        \ a b c -> (char a <|> char b) `deriv` c `shouldBe` (char a `deriv` c) <|> (char b `deriv` c)

    describe "pure" $ do
      prop "has the null derivative" $
        \ a c -> pure (a :: Char) `deriv` (c :: Char) `shouldBe` empty

    describe "ret" $ do
      prop "annihilates" $
        \ a c -> ret (a :: String) `deriv` (c :: Char) `shouldBe` empty

    describe "label" $ do
      prop "distributivity" $
        \ c s -> ((char c `label` s) `deriv` c) `shouldBe` ((char c `deriv` c) `label` s)

    describe "cat" $ do
      prop "does not pass through non-nullable lexers" $
        \ c -> (char c `cat` pure (succ c)) `deriv` succ c `shouldBe` (empty `cat` char (succ c) <|> empty `cat` pure (succ c))

      prop "passes through nullable lexers" $
        \ c d -> (ret [c :: Char] `cat` char d) `deriv` d `shouldBe` (empty `cat` char d <|> ret [c] `cat` ret [d])

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
      size lexer `shouldBe` 11

  describe "grammar" $ do
    it "lexes a sequence of characters into a sequence of characters" $
      sexprL `lex` "()" `shouldBe` [[ OpenT, CloseT ]]


data LamT = Lambda | Dot | Identifier String

lexer :: Lexer Char LamT
lexer
    = (Lambda <$ char '\\' `label` "lambda")
  <|> (Dot <$ char '.' `label` "dot")
  <|> (Identifier <$> string "x" `label` "identifier")


data SexprT = OpenT | CloseT | AtomT String
  deriving (Eq, Show)

sexprL :: Lexer Char [SexprT]
sexprL
  = many space
  *> many (((AtomT .) . (:) <$> letter <*> many alphaNum
  <|> OpenT <$ char '('
  <|> CloseT <$ char ')')
  <* many space)
