module Derivative.Parser.Spec where

import Derivative.Parser
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "Lit" $ do
    prop "produces matching characters" $
      \ c -> parseNull (Lit c `deriv` c) `shouldBe` [c]

    prop "fails on unmatched characters" $
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
