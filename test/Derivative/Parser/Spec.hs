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
        \ a b -> parseNull (pure a `cat` pure b) `shouldBe` [(a, b) :: (Char, Char)]

      prop "is empty when its left operand is empty" $
        \ b -> parseNull (nul `cat` pure b) `shouldBe` ([] :: [(Char, Char)])

      prop "is empty when its right operand is empty" $
        \ a -> parseNull (pure a `cat` nul) `shouldBe` ([] :: [(Char, Char)])

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
        \ c -> parseNull (fmap succ (lit c) `deriv` c) `shouldBe` [succ c]

    describe "lit" $ do
      prop "is empty" $
        \ a -> parseNull (lit a) `shouldBe` []

    describe "pure" $ do
      prop "returns parse trees" $
        \ a -> parseNull (pure (a :: Char)) `shouldBe` [a]

    describe "nul" $ do
      it "is empty" $
        parseNull (nul :: Parser Char) `shouldBe` []

    describe "eps" $ do
      it "is empty" $
        parseNull (eps :: Parser Char) `shouldBe` []

  describe "deriv" $ do
    describe "many" $ do
      prop "produces a list of successful parses" $
        \ c -> parseNull (many (lit c) `deriv` c) `shouldBe` [[c]]

      prop "produces no parse trees when unsuccessful" $
        \ c -> parseNull (many (lit c) `deriv` succ c) `shouldBe` []

    describe "fmap" $ do
      prop "distributes over Map" $
        \ f c -> parseNull (fmap (getBlind f :: Char -> Char) (pure c)) `shouldBe` [getBlind f c]

    describe "lit" $ do
      prop "produces matching characters" $
        \ c -> parseNull (lit c `deriv` c) `shouldBe` [c]

      prop "fails on unmatched characters" $
        \ c -> parseNull (lit c `deriv` succ c) `shouldBe` []

    describe "pure" $ do
      prop "has the null derivative" $
        \ a c -> parseNull (pure (a :: Char) `deriv` c) `shouldBe` []


  describe "Functor" $ do
    prop "obeys the identity law" $
      \ c -> parseNull (fmap id (lit c) `deriv` c) `shouldBe` parseNull (lit c `deriv` c)

    prop "obeys the composition law" $
      \ c f g -> parseNull (fmap (getBlind f :: Char -> Char) (fmap (getBlind g) (lit c)) `deriv` c) `shouldBe` parseNull (fmap (getBlind f . getBlind g) (lit c) `deriv` c)


  describe "Applicative" $ do
    prop "obeys the identity law" $
      \ v -> parseNull (pure id <*> lit v `deriv` v) `shouldBe` parseNull (lit v `deriv` v)

    prop "obeys the composition law" $
      \ u v w -> parseNull (pure (.) <*> (getBlind u :: Parser (Char -> Char)) <*> getBlind v <*> lit w `deriv` w) `shouldBe` parseNull (getBlind u <*> (getBlind v <*> lit w) `deriv` w)

    prop "obeys the homomorphism law" $
      \ x f -> parseNull (pure (getBlind f :: Char -> Char) <*> pure x) `shouldBe` parseNull (pure (getBlind f x))

    prop "obeys the interchange law" $
      \ u y -> parseNull ((getBlind u :: Parser (Char -> Char)) <*> pure y) `shouldBe` parseNull (pure ($ y) <*> getBlind u)


  describe "Alternative" $ do
    prop "obeys the some law" $
      \ v -> parseNull (some (getBlind v :: Parser Char)) `shouldBe` parseNull ((:) <$> getBlind v <*> many (getBlind v))

    prop "obeys the many law" $
      \ v -> parseNull (many (lit v)) `shouldBe` parseNull (some (lit v) <|> pure "")

    describe "(<|>)" $ do
      prop "is not right-biased" $
        \ c -> parseNull ((lit c <|> lit (succ c)) `deriv` c) `shouldBe` [c]

      prop "is not left-biased" $
        \ c -> parseNull ((lit (succ c) <|> lit c) `deriv` c) `shouldBe` [c]

      prop "returns ambiguous parses" $
        \ c -> parseNull ((lit c <|> lit c) `deriv` c) `shouldBe` [c, c]


  describe "Monad" $ do
    prop "obeys the left identity law" $
      \ k a -> parseNull (return (a :: Char) >>= getBlind k) `shouldBe` parseNull (getBlind k a :: Parser Char)

    prop "obeys the right identity law" $
      \ m -> parseNull (getBlind m >>= return) `shouldBe` parseNull (getBlind m :: Parser Char)


  describe "Show" $ do
    it "shows concatenations" $
      show (lit 'a' `cat` lit 'b') `shouldBe` "lit 'a' `cat` lit 'b'"

    it "terminates for cyclic grammars" $
      show cyclic `shouldBe` "cyclic `label` \"cyclic\""

    it "terminates for cyclic grammars2" $
      show cyclic2 `shouldBe` "Mu (\n  a => a `label` \"cyclic2\"\n)\n"


  describe "size" $ do
    prop "is 1 for terminals" $
      \ a b -> let terminals = [ ret a, lit b, nul, eps ] in sum (size <$> terminals) `shouldBe` length terminals

    prop "is 1 + the sum for nonterminals" $
      \ a b -> let binary = [ (size .) . cat, (size .) . (<|>) ]
                   unary = [ size . fmap id, size . (>>= return), size . many, size . (`label` "") ] in
        (binary <*> [ lit a ] <*> [ lit b ]) ++ (unary <*> [ lit a ]) `shouldBe` (3 <$ binary) ++ (2 <$ unary)

    it "terminates on labeled cyclic grammars" $
      size cyclic `shouldBe` 1

  describe "size2" $ do
    it "terminates on unlabelled acyclic grammars" $
      size2 (HDown (In (lit2 'c'))) `shouldBe` 1

    it "terminates on labeled cyclic grammars" $
      size2 cyclic2 `shouldBe` 1

    it "terminates on interesting cyclic grammars" $
      size2 lam2 `shouldBe` 32

  describe "parseNull2" $ do
    it "terminates on cyclic grammars" $
      let grammar = mu (\ a -> Var a `Alt` In (Ret ["x"])) in
      parseNull2 grammar `shouldBe` ["x"]

  describe "deriv2" $ do
    it "terminates on acyclic grammars" $
      HDown (In (Lit 'x')) `deriv2` 'x' `shouldBe` HDown (In (ret2 "x"))

    it "deriv2 terminates on cyclic grammars" $
      compact2 (lam2 `deriv2` 'x') `shouldBe` HDown (In (ret2 [ Var' "x" ]))

  describe "grammar" $ do
    it "parses a literal ‘x’ as a variable name" $
      varName `parse` "x" `shouldBe` ["x"]

    it "parses whitespace one character at a time" $
      parseNull (ws `deriv` ' ') `shouldBe` " "

    it "parses whitespace a single character string of whitespace" $
      ws `parse` " " `shouldBe` " "

    it "parses repeated whitespace strings" $
      many ws `parse` "   " `shouldBe` [ "   " ]

    it "maps parse forests into abstract syntax trees" $
      var `parse` "x" `shouldBe` [ Var' "x" ]

    it "the derivative terminates on cyclic grammars" $
      (do { x <- return $! (deriv $! lam) 'x' ; x `seq` return True } ) `shouldReturn` True

    it "compaction terminates on cyclic grammars" $
      (do { x <- return $! compact $! (lam `deriv` 'x') ; x `seq` return True } ) `shouldReturn` True

    it "parseNull terminates on cyclic grammars" $
      pendingWith "this does not yet terminate"
      -- parseNull (lam `deriv` 'x') `shouldBe` [ Var' "x" ]

    it "parseNull2 terminates on cyclic grammars" $
      pendingWith "don’t have `deriv2` yet"
      -- parseNull (lam2 `deriv2` 'x') `shouldBe` [ Var' "x" ]


-- Grammar

cyclic :: Parser ()
cyclic = cyclic `label` "cyclic"

cyclic2 :: Parser2 ()
cyclic2 = mu $ \ v -> Var v `Lab` "cyclic2"

varName :: Parser String
varName = literal "x"

ws :: Parser Char
ws = oneOf (lit <$> " \t\r\n") `label` "ws"

var :: Parser Lam
var = Var' <$> varName `label` "var"

abs :: Parser Lam
abs = Abs <$> (literal "\\" *> optional ws *> varName <* optional ws) <*> (lit '.' *> optional ws *> lam) `label` "abs"

app :: Parser Lam
app = App <$> lam <*> (ws *> lam) `label` "app"

lam :: Parser Lam
lam = abs <|> var <|> app `label` "lambda"

lam2 :: Parser2 Lam
lam2 = mu (\ lam ->
  let var = In ((Var' . pure) `Map` In (Lit 'x')) `Lab` "var"
      app = (In (App `Map` Var lam) <*> (In (Lit ' ') *> Var lam)) `Lab` "app"
      abs = (In (Abs `Map` (In (literal2 "\\") *> (In $ pure `Map` In (Lit 'x')))) <*> (In (Lit '.') *> Var lam)) `Lab` "abs" in
      In (In (In abs `Alt` In var) `Alt` In app) `Lab` "lambda")


-- Types

data Lam = Var' String | Abs String Lam | App Lam Lam
  deriving (Eq, Show)


-- Instances

instance Arbitrary a => Arbitrary (Parser a) where
  arbitrary = oneof
    [ pure <$> arbitrary
    , pure nul
    , pure eps
    ]
