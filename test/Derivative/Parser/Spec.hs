{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Derivative.Parser.Spec where

import Control.Applicative
import Derivative.Parser
import Prelude hiding (abs)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (label)

{-# ANN module "HLint: ignore Redundant do" #-}
{-# ANN module "HLint: ignore Functor law" #-}
{-# ANN module "HLint: ignore Monad law, left identity" #-}
{-# ANN module "HLint: ignore Monad law, right identity" #-}

spec :: Spec
spec = do
  describe "parseNull" $ do
    describe "cat" $ do
      prop "returns pairs of its parse trees" $
        \ a b -> parseNull (HDown $ pure a `cat` pure b) `shouldBe` [(a, b) :: (Char, Char)]

      prop "is empty when its left operand is empty" $
        \ b -> parseNull (HDown $ nul `cat` pure b) `shouldBe` ([] :: [(Char, Char)])

      prop "is empty when its right operand is empty" $
        \ a -> parseNull (HDown $ pure a `cat` nul) `shouldBe` ([] :: [(Char, Char)])

    describe "<|>" $ do
      prop "returns left parse trees" $
        \ a -> parseNull (pure a <|> empty) `shouldBe` [a :: Char]

      prop "returns right parse trees" $
        \ b -> parseNull (empty <|> pure b) `shouldBe` [b :: Char]

      prop "returns ambiguous parse trees" $
        \ a b -> parseNull (pure a <|> pure b) `shouldBe` [a, b :: Char]

    describe "many" $ do
      prop "contains the empty sequence" $
        \ p -> parseNull (many (getBlind p :: Parser Char)) `shouldBe` [[]]

    describe "fmap" $ do
      prop "applies a function to its parse trees" $
        \ c -> parseNull (fmap succ (HDown $ lit c) `deriv` c) `shouldBe` [succ c]

    describe "lit" $ do
      prop "is empty" $
        \ a -> parseNull (HDown $ lit a) `shouldBe` []

    describe "pure" $ do
      prop "returns parse trees" $
        \ a -> parseNull (pure (a :: Char)) `shouldBe` [a]

    describe "nul" $ do
      it "is empty" $
        parseNull (HDown nul :: Parser Char) `shouldBe` []

    describe "eps" $ do
      it "is empty" $
        parseNull (HDown eps :: Parser Char) `shouldBe` []

    it "terminates on cyclic grammars" $
      let grammar = mu (\ a -> a <|> ret ["x"]) in
      parseNull grammar `shouldBe` ["x"]

    it "terminates on cyclic grammars" $
      parseNull (lam `deriv` 'x') `shouldBe` [ Var' "x" ]

  describe "deriv" $ do
    describe "many" $ do
      prop "produces a list of successful parses" $
        \ c -> parseNull (HDown (many (lit c)) `deriv` c) `shouldBe` [[c]]

      prop "produces no parse trees when unsuccessful" $
        \ c -> parseNull (HDown (many (lit c)) `deriv` succ c) `shouldBe` []

    describe "fmap" $ do
      prop "distributivity" $
        \ f c -> parseNull (fmap (getBlind f :: Char -> Char) (pure c)) `shouldBe` [getBlind f c]

    describe "lit" $ do
      prop "represents unmatched content with the nul parser" $
        \ a -> HDown (lit a) `deriv` succ a `shouldBe` HDown nul

      prop "represents matched content with ε reduction parsers" $
        \ a -> HDown (lit a) `deriv` a `shouldBe` HDown (ret [a])

    describe "<|>" $ do
      prop "distributivity" $
        \ a b c -> HDown (lit a <|> lit b) `deriv` c `shouldBe` (HDown (lit a) `deriv` c) <|> (HDown (lit b) `deriv` c)

    describe "pure" $ do
      prop "has the null derivative" $
        \ a c -> parseNull (pure (a :: Char) `deriv` c) `shouldBe` []

    it "terminates on cyclic grammars" $
      lam `deriv` 'x' `shouldBe` HDown (ret [ Var' "x" ])

    describe "ret" $ do
      prop "annihilates" $
        \ a c -> HDown (ret (a :: String)) `deriv` c `shouldBe` HDown nul


  describe "Functor" $ do
    prop "obeys the identity law" $
      \ c -> parseNull (fmap id (HDown $ lit c) `deriv` c) `shouldBe` parseNull ((HDown $ lit c) `deriv` c)

    prop "obeys the composition law" $
      \ c f g -> parseNull (fmap (getBlind f :: Char -> Char) (fmap (getBlind g) (HDown $ lit c)) `deriv` c) `shouldBe` parseNull (fmap (getBlind f . getBlind g) (HDown $ lit c) `deriv` c)


  describe "Applicative" $ do
    prop "obeys the identity law" $
      \ v -> parseNull (pure id <*> HDown (lit v) `deriv` v) `shouldBe` parseNull ((HDown $ lit v) `deriv` v)

    prop "obeys the composition law" $
      \ u v w -> parseNull (pure (.) <*> (getBlind u :: Parser (Char -> Char)) <*> getBlind v <*> (HDown $ lit w) `deriv` w) `shouldBe` parseNull (getBlind u <*> (getBlind v <*> (HDown $ lit w)) `deriv` w)

    prop "obeys the homomorphism law" $
      \ x f -> parseNull (pure (getBlind f :: Char -> Char) <*> pure x) `shouldBe` parseNull (pure (getBlind f x))

    prop "obeys the interchange law" $
      \ u y -> parseNull ((getBlind u :: Parser (Char -> Char)) <*> pure y) `shouldBe` parseNull (pure ($ y) <*> getBlind u)


  describe "Alternative" $ do
    prop "obeys the some law" $
      \ v -> parseNull (some (getBlind v :: Parser Char)) `shouldBe` parseNull ((:) <$> getBlind v <*> many (getBlind v))

    prop "obeys the many law" $
      \ v -> parseNull (many (HDown $ lit v)) `shouldBe` parseNull (some (HDown $ lit v) <|> pure "")

    describe "(<|>)" $ do
      prop "is not right-biased" $
        \ c -> parseNull ((HDown $ lit c <|> lit (succ c)) `deriv` c) `shouldBe` [c]

      prop "is not left-biased" $
        \ c -> parseNull ((HDown $ lit (succ c) <|> lit c) `deriv` c) `shouldBe` [c]

      prop "returns ambiguous parses" $
        \ c -> parseNull ((HDown $ lit c <|> lit c) `deriv` c) `shouldBe` [c, c]


  describe "Monad" $ do
    prop "obeys the left identity law" $
      \ k a -> parseNull (return (a :: Char) >>= getBlind k) `shouldBe` parseNull (getBlind k a :: Parser Char)

    prop "obeys the right identity law" $
      \ m -> parseNull (getBlind m >>= return) `shouldBe` parseNull (getBlind m :: Parser Char)


  describe "Show" $ do
    it "shows concatenations" $
      show (HDown $ lit 'a' `cat` lit 'b') `shouldBe` "lit 'a' `cat` lit 'b'"

    it "terminates for cyclic grammars" $
      show cyclic `shouldBe` "Mu (\n  a => a `label` \"cyclic\"\n)\n"


  describe "size" $ do
    prop "is 1 for terminals" $
      \ a b -> let terminals = [ HDown $ ret a, HDown $ lit b, HDown nul, HDown eps ] in sum (size <$> terminals) `shouldBe` length terminals

    prop "is 1 + the sum for unary nonterminals" $
      \ a s -> [ size (HDown (fmap id (lit a))), size (HDown (lit a >>= return)), size (HDown (lit a `label` s)) ] `shouldBe` [ 2, 2, 2 ]

    prop "is 1 + the sum for binary nonterminals" $
      \ a b -> [ size (HDown (lit a `cat` lit b)), size (HDown (lit a <|> lit b)) ] `shouldBe` [ 3, 3 ]

    it "terminates on unlabelled acyclic grammars" $
      size (HDown (lit 'c')) `shouldBe` 1

    it "terminates on labeled cyclic grammars" $
      size cyclic `shouldBe` 1

    it "terminates on interesting cyclic grammars" $
      size lam `shouldBe` 32

  describe "grammar" $ do
    it "parses a literal ‘x’ as a variable name" $
      varName `parse` "x" `shouldBe` ["x"]

    it "parses whitespace one character at a time" $
      parseNull (ws `deriv` ' ') `shouldBe` " "

    it "parses whitespace a single character string of whitespace" $
      ws `parse` " " `shouldBe` " "

    it "parses repeated whitespace strings" $
      many ws `parse` "   " `shouldBe` [ "   " ]

    it "the derivative terminates on cyclic grammars" $
      (do { x <- return $! (deriv $! lam) 'x' ; x `seq` return True } ) `shouldReturn` True

    it "compaction terminates on cyclic grammars" $
      (do { x <- return $! compact $! (lam `deriv` 'x') ; x `seq` return True } ) `shouldReturn` True


-- Grammar

cyclic :: Parser ()
cyclic = mu $ \ v -> v `label` "cyclic"

varName :: Parser String
varName = HDown $ literal "x"

ws :: Parser Char
ws = HDown $ oneOf (lit <$> " \t\r\n") `label` "ws"

lam :: Parser Lam
lam = mu (\ lam ->
  let var = Var' . pure <$> lit 'x' `label` "var"
      app = (App <$> lam <*> (lit ' ' *> lam)) `label` "app"
      abs = (Abs <$> (literal "\\" *> (pure <$> lit 'x')) <*> (lit '.' *> lam)) `label` "abs" in
      abs <|> var <|> app `label` "lambda")


-- Types

data Lam = Var' String | Abs String Lam | App Lam Lam
  deriving (Eq, Show)


-- Instances

instance Arbitrary a => Arbitrary (HGraph ParserF a) where
  arbitrary = oneof
    [ pure <$> arbitrary
    , pure (HDown nul)
    , pure (HDown eps)
    ]
