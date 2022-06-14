{-# LANGUAGE FlexibleInstances, GADTs, RankNTypes, ScopedTypeVariables #-}
module Derivative.Parser
( compact
, deriv
, parse
, parseNull
, PatternF(..)
, module Pattern
, Parser
, Combinator
, size
, Derivative.Parser.mu
, parser
, combinator
, nullable
) where

import Control.Applicative
import Data.Foldable (foldl')
import Data.Functor.K
import Data.Higher.Foldable
import Data.Higher.Graph as Graph
import Data.Monoid hiding (Alt)
import Data.Pattern as Pattern
import Data.Predicate

-- API

parse :: Foldable f => Parser t a -> f t -> [a]
parse p = parseNull . foldl' deriv (compact p)


mu :: (Combinator t v a -> Combinator t v a) -> Combinator t v a
mu f = Rec $ Mu $ \ v -> case f (Var v) of
  Rec (In r) -> r
  p -> p `Lab` ""


parser :: (forall v. Combinator t v a) -> Parser t a
parser r = compact $ Graph r

combinator :: Parser t a -> Combinator t v a
combinator p = unGraph p

-- Types

type Parser t = Graph (PatternF t)
type Combinator t = Rec (PatternF t)


-- Algorithm

deriv :: forall a t. Parser t a -> t -> Parser t a
deriv g c = Graph (deriv' (combinator g))
  where deriv' :: forall a v. Combinator t (Combinator t v) a -> Combinator t v a
        deriv' rc = case rc of
          Var v -> v
          Rec (Mu g) -> deriv'' (g (pjoin (Graph.mu g)))
          Rec (In r) -> deriv'' r
        deriv'' :: forall a v. PatternF t (Combinator t (Graph.Rec (PatternF t) v)) a -> Combinator t v a
        deriv'' p = case p of
          Cat a b -> deriv' a `cat` pjoin b <|> delta (pjoin a) `cat` deriv' b
          Alt a b -> deriv' a <|> deriv' b
          Rep a -> uncurry (:) <$> (deriv' a `cat` pjoin (many a))
          Map f p -> f <$> deriv' p
          Bnd p f -> deriv' p >>= pjoin . f
          Sat p -> if c `satisfies` p then pure c else empty
          Mat p -> case p c of { Just a -> pure a ; _ -> empty }
          Lab p s -> deriv' p `label` s
          _ -> empty

parseNull :: Parser t a -> [a]
parseNull = (`fold` []) $ \ parser -> case parser of
  Cat a b -> (,) <$> a <*> b
  Alt a b -> a <> b
  Rep _ -> [[]]
  Map f p -> f <$> p
  Bnd p f -> p >>= f
  Ret as -> as
  Lab p _ -> p
  Del a -> a
  _ -> []

compact :: Parser t a -> Parser t a
compact = transform compactF

nullable :: Parser t a -> Bool
nullable = (getConst .) $ (`fold` Const False) $ \ p -> case p of
  Cat a b -> Const $ getConst a && getConst b
  Alt a b -> Const $ getConst a || getConst b
  Rep _ -> Const True
  Map _ p -> Const (getConst p)
  Bnd p _ -> Const (getConst p)
  Ret _ -> Const True
  Lab p _ -> p
  Del a -> a
  _ -> Const False


size :: Parser t a -> Int
size = getSum . getK . fold ((K (Sum 1) <|>) . hfoldMap id) (K (Sum 0))
