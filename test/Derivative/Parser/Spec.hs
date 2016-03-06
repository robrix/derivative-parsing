module Derivative.Parser.Spec where

import Test.Hspec

spec :: Spec
spec = do
  describe "fmap" $ do
    it "does a thing" $ True `shouldBe` True


-- Types

data Lam = Var String | Abs String Lam | App' Lam Lam
  deriving (Eq, Show)
