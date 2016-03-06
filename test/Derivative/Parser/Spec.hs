module Derivative.Parser.Spec where

import Control.Applicative
import Derivative.Parser
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "parseNull" $ do
    describe "Lit" $ do
      prop "produces matching characters" $
        \ c -> parseNull (Lit c `deriv` c) `shouldBe` [c]

      prop "fails on unmatched characters" $
        \ c -> parseNull (Lit c `deriv` succ c) `shouldBe` []

    describe "Ret" $ do
      prop "returns parse trees" $
        \ a -> parseNull (Ret [a :: Char]) `shouldBe` [a]

      prop "has the null derivative" $
        \ a c -> parseNull (Ret [a :: Char] `deriv` c) `shouldBe` []

    describe "Alt" $ do
      prop "returns left parse trees in Left" $
        \ a -> parseNull (pure a `Alt` empty) `shouldBe` [Left a :: Either Char Char]

      prop "returns right parse trees in Right" $
        \ b -> parseNull (empty `Alt` pure b) `shouldBe` [Right b :: Either Char Char]

      prop "returns ambiguous parse trees" $
        \ a b -> parseNull (pure a `Alt` pure b) `shouldBe` [Left a, Right b :: Either Char Char]

    describe "Map" $ do
      prop "applies a function to its parse trees" $
        \ c -> parseNull (Map succ (Lit c) `deriv` c) `shouldBe` [succ c]

  describe "Functor" $ do
    prop "obeys the identity law" $
      \ c -> parseNull (fmap id (Lit c) `deriv` c) `shouldBe` parseNull (Lit c `deriv` c)

    prop "obeys the composition law" $
      \ c f g -> parseNull (fmap (getBlind f :: Char -> Char) (fmap (getBlind g) (Lit c)) `deriv` c) `shouldBe` parseNull (fmap (getBlind f . getBlind g) (Lit c) `deriv` c)

  describe "Applicative" $ do
    prop "obeys the identity law" $
      \ v -> parseNull (pure id <*> Lit v `deriv` v) `shouldBe` parseNull (Lit v `deriv` v)

    prop "obeys the composition law" $
      \ u v w -> parseNull (pure (.) <*> (getBlind u :: Parser (Char -> Char)) <*> getBlind v <*> Lit w `deriv` w) `shouldBe` parseNull (getBlind u <*> (getBlind v <*> Lit w) `deriv` w)

    prop "obeys the homomorphism law" $
      \ x f -> parseNull (pure (getBlind f :: Char -> Char) <*> pure x) `shouldBe` parseNull (pure (getBlind f x))

    prop "obeys the interchange law" $
      \ u y -> parseNull ((getBlind u :: Parser (Char -> Char)) <*> pure y) `shouldBe` parseNull (pure ($ y) <*> getBlind u)

  describe "Alternative" $ do
    prop "obeys the some law" $
      \ v -> parseNull (some (getBlind v :: Parser Char)) `shouldBe` parseNull ((:) <$> getBlind v <*> many (getBlind v))

  describe "grammar" $ do
    it "parses a literal ‘x’ as a variable name" $
      varName `parse` "x" `shouldBe` ["x"]


-- Grammar

varName :: Parser String
varName = literal "x"


-- Types

data Lam = Var String | Abs String Lam | App' Lam Lam
  deriving (Eq, Show)


-- Instances

instance Arbitrary a => Arbitrary (Parser a) where
  arbitrary = oneof
    [ pure <$> arbitrary
    , pure Nul
    , pure Eps
    ]
