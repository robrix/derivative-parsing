module Derivative.Parser.Spec where

import Derivative.Parser
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "deriv" $ do
    prop "returns matching characters from literal parsers" $
      \ c -> parseNull (Lit c `deriv` c) `shouldBe` [c]

    prop "returns the null parser for unmatched characters" $
      \ c -> parseNull (Lit c `deriv` succ c) `shouldBe` []

  describe "grammar" $ do
    it "parses a literal ‘x’ as a variable name" $
      varName `parse` "x" `shouldBe` ["x"]


-- Grammar

varName :: Parser String
varName = literal "x"


-- Types

data Lam = Var String | Abs String Lam | App' Lam Lam
  deriving (Eq, Show)
