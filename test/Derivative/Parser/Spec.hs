module Derivative.Parser.Spec where

import Derivative.Parser
import Test.Hspec

spec :: Spec
spec = do
  describe "deriv" $ do
    it "returns matching characters from literal parsers" $
      parseNull (Lit 'c' `deriv` 'c') `shouldBe` "c"

  describe "grammar" $ do
    it "parses a literal ‘x’ as a variable name" $
      varName `parse` "x" `shouldBe` ["x"]


-- Grammar

varName :: Parser String
varName = literal "x"


-- Types

data Lam = Var String | Abs String Lam | App' Lam Lam
  deriving (Eq, Show)
