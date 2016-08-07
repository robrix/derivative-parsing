module Derivative.Lexer.Spec where

import Derivative.Lexer
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "parseNull" $ do
    describe "cat" $ do
      prop "returns pairs of its parse trees" $
        \ a b -> parseNull (pure a `cat` pure b) `shouldBe` [(a, b) :: (Char, Char)]
