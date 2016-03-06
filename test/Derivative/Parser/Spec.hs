module Derivative.Parser.Spec where

import Derivative.Parser
import Test.Hspec

spec :: Spec
spec = do
  describe "fmap" $ do
    it "does a thing" $ True `shouldBe` True


-- Grammar

varName :: Parser String
varName = literal "x"


-- Types

data Lam = Var String | Abs String Lam | App' Lam Lam
  deriving (Eq, Show)
