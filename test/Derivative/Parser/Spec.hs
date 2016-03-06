module Derivative.Parser.Spec where

import Derivative.Parser
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
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
      \ c -> parseNull (Lit c `Alt` Lit (succ c) `deriv` c) `shouldBe` [Left c]

    prop "returns right parse trees in Right" $
      \ c -> parseNull (Lit (succ c) `Alt` Lit c `deriv` c) `shouldBe` [Right c]

    prop "returns ambiguous parse trees" $
      \ c -> parseNull (Lit c `Alt` Lit c `deriv` c) `shouldBe` [Left c, Right c]

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
      \ u v w -> parseNull (pure (.) <*> pure (getBlind u :: Char -> Char) <*> pure (getBlind v :: Char -> Char) <*> Lit w `deriv` w) `shouldBe` parseNull (pure (getBlind u) <*> (pure (getBlind v) <*> Lit w) `deriv` w)

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
  arbitrary = pure <$> arbitrary
