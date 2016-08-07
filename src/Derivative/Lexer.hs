{-# LANGUAGE FlexibleInstances, GADTs, ScopedTypeVariables #-}
module Derivative.Lexer
( Lexer
, lex
, deriv
, parseNull
, compact
, size
, module Pattern
) where

import Control.Applicative
import Data.Foldable
import Data.Functor.K
import Data.Higher.Foldable
import Data.Higher.Functor.Fix
import Data.Monoid hiding (Alt)
import Data.Pattern as Pattern
import Data.Predicate
import Prelude hiding (lex)

-- API

lex :: Foldable f => Lexer t a -> f t -> [a]
lex p = parseNull . foldl' deriv (compact p)


-- Types

type Lexer t = Fix (PatternF t)


-- Algorithm

deriv :: forall a t. Lexer t a -> t -> Lexer t a
deriv g c = deriv' g
  where deriv' :: forall a. Lexer t a -> Lexer t a
        deriv' (Fix p) = case p of
          Cat a b -> deriv' a `cat` b <|> delta a `cat` deriv' b
          Alt a b -> deriv' a <|> deriv' b
          Rep a -> uncurry (:) <$> (deriv' a `cat` many a)
          Map f p -> f <$> deriv' p
          Bnd p f -> deriv' p >>= f
          Sat p -> if c `satisfies` p then pure c else empty
          Lab p s -> deriv' p `label` s
          _ -> empty

parseNull :: Lexer t a -> [a]
parseNull = cata $ \ parser -> case parser of
  Cat a b -> (,) <$> a <*> b
  Alt a b -> a <> b
  Rep _ -> [[]]
  Map f p -> f <$> p
  Bnd p f -> p >>= f
  Ret as -> as
  Lab p _ -> p
  Del a -> a
  _ -> []


compact :: Lexer t a -> Lexer t a
compact = Fix . compactF . unFix


size :: Lexer t a -> Int
size = getSum . getK . cata ((K (Sum 1) <|>) . hfoldMap id)
