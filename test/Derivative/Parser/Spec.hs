{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Derivative.Parser.Spec where

import Control.Applicative
import Control.Monad
import Data.Pattern.Char
import Data.Pattern.Char.Spec
import Derivative.Lexer hiding (compact, size, deriv)
import Derivative.Parser
import Prelude hiding (abs, lex)
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
        \ a b -> parseNull (parser $ pure a `cat` pure b) `shouldBe` [(a, b) :: (Char, Char)]

      prop "is empty when its left operand is empty" $
        \ b -> parseNull (parser $ empty `cat` pure b) `shouldBe` ([] :: [(Char, Char)])

      prop "is empty when its right operand is empty" $
        \ a -> parseNull (parser $ pure a `cat` empty) `shouldBe` ([] :: [(Char, Char)])

    describe "<|>" $ do
      prop "returns left parse trees" $
        \ a -> parseNull (pure a <|> empty) `shouldBe` [a :: Char]

      prop "returns right parse trees" $
        \ b -> parseNull (empty <|> pure b) `shouldBe` [b :: Char]

      prop "returns ambiguous parse trees" $
        \ a b -> parseNull (pure a <|> pure b) `shouldBe` [a, b :: Char]

    describe "many" $ do
      prop "contains the empty sequence" $
        \ p -> parseNull (many (getBlind p :: Parser Char Char)) `shouldBe` [[]]

    describe "fmap" $ do
      prop "applies a function to its parse trees" $
        \ c -> parseNull (fmap succ (parser $ char c) `deriv` c) `shouldBe` [succ c]

    describe "char" $ do
      prop "is empty" $
        \ a -> parseNull (parser $ char a) `shouldBe` []

    describe "pure" $ do
      prop "returns parse trees" $
        \ a -> parseNull (pure (a :: Char)) `shouldBe` [a]

    describe "empty" $ do
      it "is empty" $
        parseNull (empty :: Parser Char Char) `shouldBe` []

    describe "ret" $ do
      prop "is identity" $
        \ t -> parseNull (parser (ret t) :: Parser Char Char) `shouldBe` t

    it "terminates on cyclic grammars" $
      let grammar = parser $ mu (\ a -> a <|> ret ["x"]) in
      parseNull grammar `shouldBe` ["x"]


  describe "deriv" $ do
    describe "many" $ do
      prop "produces a list of successful parses" $
        \ c -> parseNull (parser (many (char c)) `deriv` c) `shouldBe` [[c]]

      prop "produces no parse trees when unsuccessful" $
        \ c -> parseNull (parser (many (char c)) `deriv` succ c) `shouldBe` []

      prop "is constant in parser size" $
        \ c i -> let p = parser (many (char c)) in
          take i (size <$> iterate (`deriv` c) p) `shouldBe` take i (size p : repeat (succ (size p)))

    describe "fmap" $ do
      prop "distributivity" $
        \ f c -> parseNull (fmap (getBlind f :: Char -> Char) (pure c)) `shouldBe` [getBlind f c]

    describe "char" $ do
      prop "represents unmatched content with the empty parser" $
        \ a -> parser (char a) `deriv` succ a `shouldBe` empty

      prop "represents matched content with ε reduction parsers" $
        \ a -> parser (char a) `deriv` a `shouldBe` parser (ret [a])

    describe "<|>" $ do
      prop "distributivity" $
        \ a b c -> parser (char a <|> char b) `deriv` c `shouldBe` (parser (char a) `deriv` c) <|> (parser (char b) `deriv` c)

    describe "pure" $ do
      prop "has the null derivative" $
        \ a c -> pure (a :: Char) `deriv` (c :: Char) `shouldBe` empty

    it "terminates on cyclic grammars" $
      compact (lam `deriv` 'x') `shouldNotBe` empty

    describe "ret" $ do
      prop "annihilates" $
        \ a c -> parser (ret (a :: String)) `deriv` (c :: Char) `shouldBe` empty

    describe "label" $ do
      prop "distributivity" $
        \ c s -> parser (char c `label` s) `deriv` c `shouldBe` parser (combinator (parser (char c) `deriv` c) `label` s)

    describe "cat" $ do
      prop "does not pass through non-nullable parsers" $
        \ c -> parser (char c `cat` pure (succ c)) `deriv` succ c `shouldBe` parser (empty `cat` char (succ c) <|> empty `cat` pure (succ c))

      prop "passes through nullable parsers" $
        \ c d -> parser (ret [c :: Char] `cat` char d) `deriv` d `shouldBe` parser (empty `cat` char d <|> ret [c] `cat` ret [d])

      it "applies to recursive grammars" $
        parseNull (((sexpr `deriv` '(') `deriv` 'x') `deriv` ')') `shouldBe` [ List [ Atom "x" ] ]


  describe "nullable" $ do
    describe "cat" $ do
      prop "is the conjunction of its operands’ nullability" $
        \ a b -> nullable (parser (combinator a `cat` combinator b)) `shouldBe` nullable (a :: Parser Char Char) && nullable (b :: Parser Char Char)

    describe "empty" $
      it "is not nullable" $
        nullable empty `shouldBe` False

    describe "ret" $
      prop "is nullable" $
        \ c -> nullable (parser (ret (c :: String))) `shouldBe` True


  describe "compaction" $ do
    prop "reduces parser size" $
      \ p -> size (compact p :: Parser Char Char) `shouldSatisfy` (<= size p)


  describe "Functor" $ do
    prop "obeys the identity law" $
      \ c -> parseNull (fmap id (parser $ char c) `deriv` c) `shouldBe` parseNull (parser (char c) `deriv` c)

    prop "obeys the composition law" $
      \ c f g -> parseNull (fmap (getBlind f :: Char -> Char) (fmap (getBlind g) (parser $ char c)) `deriv` c) `shouldBe` parseNull (fmap (getBlind f . getBlind g) (parser $ char c) `deriv` c)


  describe "Applicative" $ do
    prop "obeys the identity law" $
      \ v -> parseNull (pure id <*> parser (char v) `deriv` v) `shouldBe` parseNull (parser (char v) `deriv` v)

    prop "obeys the composition law" $
      \ u v w -> parseNull (pure (.) <*> (getBlind u :: Parser Char (Char -> Char)) <*> getBlind v <*> parser (char w) `deriv` w) `shouldBe` parseNull (getBlind u <*> (getBlind v <*> parser (char w)) `deriv` w)

    prop "obeys the homomorphism law" $
      \ x f -> parseNull (pure (getBlind f :: Char -> Char) <*> pure x) `shouldBe` parseNull (pure (getBlind f x))

    prop "obeys the interchange law" $
      \ u y -> parseNull ((getBlind u :: Parser Char (Char -> Char)) <*> pure y) `shouldBe` parseNull (pure ($ y) <*> getBlind u)

    prop "obeys the fmap identity" $
      \ f x -> parseNull ((pure (getBlind f) :: Parser Char (Char -> Char)) <*> x) `shouldBe` parseNull (fmap (getBlind f) x)

    prop "obeys the return identity" $
      \ f -> pure (getBlind f :: Char -> Char) `shouldBe` (return (getBlind f :: Char -> Char) :: Parser Char (Char -> Char))

    prop "obeys the ap identity" $
      \ f x -> parseNull ((pure (getBlind f) :: Parser Char (Char -> Char)) <*> x) `shouldBe` parseNull (pure (getBlind f :: Char -> Char) `ap` x)

    prop "obeys the left-discarding identity" $
      \ u v -> parseNull (u *> v) `shouldBe` parseNull (pure (const id) <*> (u :: Parser Char Char) <*> (v :: Parser Char Char))

    prop "obeys the right-discarding identity" $
      \ u v -> parseNull (u <* v) `shouldBe` parseNull (pure const <*> (u :: Parser Char Char) <*> (v :: Parser Char Char))


  describe "Alternative" $ do
    prop "obeys the some law" $
      \ v -> parseNull (some (getBlind v :: Parser Char Char)) `shouldBe` parseNull ((:) <$> getBlind v <*> many (getBlind v))

    prop "obeys the many law" $
      \ v -> parseNull (many (parser $ char v)) `shouldBe` parseNull (some (parser $ char v) <|> pure "")

    describe "(<|>)" $ do
      prop "is not right-biased" $
        \ c -> parseNull (parser (char c <|> char (succ c)) `deriv` c) `shouldBe` [c]

      prop "is not left-biased" $
        \ c -> parseNull (parser (char (succ c) <|> char c) `deriv` c) `shouldBe` [c]

      prop "returns ambiguous parses" $
        \ c -> parseNull (parser (char c <|> char c) `deriv` c) `shouldBe` [c, c]


  describe "Monad" $ do
    prop "obeys the left identity law" $
      \ k a -> parseNull (return (a :: Char) >>= getBlind k) `shouldBe` parseNull (getBlind k a :: Parser Char Char)

    prop "obeys the right identity law" $
      \ m -> parseNull (getBlind m >>= return) `shouldBe` parseNull (getBlind m :: Parser Char Char)


  describe "Show" $ do
    it "shows concatenations" $
      show (parser $ char 'a' `cat` char 'b') `shouldBe` "char 'a' `cat` char 'b'"

    it "terminates for cyclic grammars" $
      show cyclic `shouldBe` "Mu (\\ a ->\n  a `label` \"cyclic\"\n)\n"

    it "does not parenthesize left-nested alternations" $
      show (parser (char 'a' <|> char 'b' <|> char 'c')) `shouldBe` "char 'a' <|> char 'b' <|> char 'c'"

    it "parenthesizes right-nested alternations" $
      show (parser (char 'a' <|> (char 'b' <|> char 'c'))) `shouldBe` "char 'a' <|> (char 'b' <|> char 'c')"


  describe "size" $ do
    prop "is 1 for terminals" $
      \ a b -> let terminals = [ parser $ ret a, parser $ char b, empty, parser (ret []) ] in sum (size <$> terminals) `shouldBe` length terminals

    prop "is 1 + the sum for unary nonterminals" $
      \ a s -> [ size (parser (fmap id (char a))), size (parser (char a >>= return)), size (parser (char a `label` s)) ] `shouldBe` [ 2, 2, 2 ]

    prop "is 1 + the sum for binary nonterminals" $
      \ a b -> [ size (parser (char a `cat` char b)), size (parser (char a <|> char b)) ] `shouldBe` [ 3, 3 ]

    it "terminates on unlabelled acyclic grammars" $
      size (parser (char 'c')) `shouldBe` 1

    it "terminates on labeled cyclic grammars" $
      size cyclic `shouldBe` 1

    it "terminates on interesting cyclic grammars" $
      size lam `shouldBe` 21


  describe "grammar" $ do
    it "parses a literal ‘x’ as a variable name" $
      varName `parse` "x" `shouldBe` ["x"]

    it "parses whitespace one character at a time" $
      parseNull (parser space `deriv` ' ') `shouldBe` " "

    it "parses a single character string of whitespace" $
      parser space `parse` " " `shouldBe` " "

    it "parses two characters of whitespace" $
      parser ((,) <$> space <*> space) `parse` "  " `shouldBe` [(' ', ' ')]

    it "parses repeated whitespace strings" $
      parser (many space) `parse` "   " `shouldBe` [ "   " ]

    it "the derivative terminates on cyclic grammars" $
      (do { x <- return $! (deriv $! lam) 'x' ; x `seq` return True } ) `shouldReturn` True

    it "compaction terminates on cyclic grammars" $
      (do { x <- return $! compact $! (lam `deriv` 'x') ; x `seq` return True } ) `shouldReturn` True

    it "parses variables" $
      lam `parse` "x" `shouldBe` [ Var' "x" ]

    it "parses a list of lexemes" $
      parse sexprP <$> (sexprL `lex` "(() a (b c))") `shouldBe` [ [ List [ List [], Atom "a", List [ Atom "b", Atom "c" ] ] ] ]

  describe "Char" Data.Pattern.Char.Spec.spec


-- Grammar

cyclic :: Parser Char ()
cyclic = parser $ mu $ \ v -> v `label` "cyclic"

varName :: Parser Char String
varName = parser $ string "x"

lam :: Parser Char Lam
lam = parser $ mu (\ lam ->
  let var = Var' . pure <$> char 'x' `label` "var"
      app = (App <$> lam <*> (char ' ' *> lam)) `label` "app"
      abs = (Abs . pure <$> (char '\\' *> char 'x') <*> (char '.' *> lam)) `label` "abs" in
      abs <|> var <|> app) `label` "lambda"

sexpr :: Parser Char Sexpr
sexpr = parser $ Derivative.Parser.mu (\ a ->
      Atom <$> identifier
  <|> List <$> (char open *> sep (char ' ') a <* char close))
  where (open, close) = ('(', ')')
        identifier :: Combinator Char v String
        identifier = (:) <$> letter <*> many alphaNum

sexprL :: Lexer Char [SexprT]
sexprL
  = many space
  *> many (((AtomT .) . (:) <$> letter <*> many alphaNum
  <|> OpenT <$ char '('
  <|> CloseT <$ char ')')
  <* many space)

sexprP :: Parser SexprT Sexpr
sexprP = parser $ Derivative.Parser.mu (\ a ->
      Atom <$> match (\ t -> case t of { AtomT a -> Just a ; _ -> Nothing })
  <|> List <$> (token OpenT *> many a) <* token CloseT)


-- Types

data Lam = Var' String | Abs String Lam | App Lam Lam
  deriving (Eq, Show)

data Sexpr
  = Atom String
  | List [Sexpr]
  deriving (Eq, Show)

data SexprT = OpenT | CloseT | AtomT String
  deriving (Eq, Show)


-- Instances

instance Arbitrary a => Arbitrary (Parser t a) where
  arbitrary = oneof
    [ pure <$> arbitrary
    , pure empty
    , pure (parser (ret []))
    , (<|>) <$> arbitrary <*> arbitrary
    , (\ p s -> parser (combinator p `label` s)) <$> arbitrary <*> arbitrary
    ]
