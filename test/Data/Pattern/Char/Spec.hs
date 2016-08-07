module Data.Pattern.Char.Spec where

import Data.Char
import Data.Pattern.Char
import Derivative.Parser
import Test.Hspec
import Test.Hspec.QuickCheck

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
  describe "space" $ do
    prop "parses isSpace characters" $
      \ c -> isSpace c `shouldNotBe` null (parser space `parse` [c])

  describe "upper" $ do
    prop "parses isUpper characters" $
      \ c -> isUpper c `shouldNotBe` null (parser upper `parse` [c])

  describe "lower" $ do
    prop "parses isLower characters" $
      \ c -> isLower c `shouldNotBe` null (parser lower `parse` [c])

  describe "alphaNum" $ do
    prop "parses isAlphaNum characters" $
      \ c -> isAlphaNum c `shouldNotBe` null (parser alphaNum `parse` [c])

  describe "letter" $ do
    prop "parses isLetter characters" $
      \ c -> isLetter c `shouldNotBe` null (parser letter `parse` [c])
