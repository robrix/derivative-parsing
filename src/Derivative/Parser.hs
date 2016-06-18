{-# LANGUAGE FlexibleInstances, GADTs, RankNTypes, TypeOperators #-}
module Derivative.Parser
( cat
, commaSep
, commaSep1
, compact
, deriv
, eps
, label
, lit
, literal
, nul
, oneOf
, parse
, parseNull
, ParserF(..)
, Parser
, Combinator
, ret
, sep
, sep1
, size
, mu
, parser
, combinator
) where

import Control.Applicative
import Data.Higher.Foldable
import Data.Higher.Functor
import Data.Higher.Functor.Eq
import Data.Higher.Functor.Show
import Data.Higher.Graph
import Data.Higher.Isofunctor
import Data.Higher.Product
import Data.Higher.Transformation
import qualified Data.Monoid as Monoid
import Data.Monoid hiding (Alt)

-- API

parse :: Parser a -> String -> [a]
parse p = parseNull . foldl ((compact .) . deriv) p


commaSep1 :: Parser a -> Parser [a]
commaSep1 = sep1 (Graph (lit ','))

commaSep :: Parser a -> Parser [a]
commaSep = sep (Graph (lit ','))

sep1 :: Parser sep -> Parser a -> Parser [a]
sep1 s p = (:) <$> p <*> many (s *> p)

sep :: Parser sep -> Parser a -> Parser [a]
sep s p = s `sep1` p <|> pure []

oneOf :: (Foldable t, Alternative f) => t (f a) -> f a
oneOf = getAlt . foldMap Monoid.Alt

cat :: Combinator v a -> Combinator v b -> Combinator v (a, b)
a `cat` b = In $ Cat a b

lit :: Char -> Combinator v Char
lit = In . Lit

ret :: [a] -> Combinator v a
ret = In . Ret

nul :: Combinator v a
nul = In Nul

eps :: Combinator v a
eps = In Eps

infixr 2 `label`

label :: Combinator v a -> String -> Combinator v a
label p = In . Lab p

literal :: String -> Combinator v String
literal string = sequenceA (In . Lit <$> string)

mu :: (forall v. Combinator v a -> Combinator v a) -> Parser a
mu f = Graph $ Mu $ \ v -> case f (Var v) of
  In r -> r
  p -> p `Lab` ""

parser :: (forall v. Combinator v a) -> Parser a
parser = Graph

combinator :: Parser a -> Combinator v a
combinator = unGraph


-- Types

-- | A parser type encoding concatenation, alternation, repetition, &c. as first-order constructors.
data ParserF f a where
  Cat :: f a -> f b -> ParserF f (a, b)
  Alt :: f a -> f a -> ParserF f a
  Rep :: f a -> ParserF f [a]
  Map :: (a -> b) -> f a -> ParserF f b
  Bnd :: f a -> (a -> f b) -> ParserF f b
  Lit :: Char -> ParserF f Char
  Ret :: [a] -> ParserF f a
  Nul :: ParserF f a
  Eps :: ParserF f a
  Lab :: f a -> String -> ParserF f a

type Parser a = Graph ParserF a
type Combinator v = Rec ParserF v


-- Algorithm

type Derivative v = (v :*: [] :*: Const Bool)

liftRec' :: HFunctor f => (forall v. Rec f (v :*: w) ~> Rec f (v :*: w)) -> (f w ~> w) -> (forall a. w a) -> forall v. Rec f v ~> Rec f v
liftRec' f algebra initial = let (into, outof) = hisomap (:*: initial) hfst in outof . f . into

liftGraph' :: HFunctor f => (forall v. Rec f (v :*: w) ~> Rec f (v :*: w)) -> (f w ~> w) -> (forall a. w a) -> Graph f ~> Graph f
liftGraph' f algebra initial = modifyGraph (liftRec' f algebra initial)

deriv :: Parser a -> Char -> Parser a
deriv g c = liftGraph' deriv' (parseNull'' `hdistribute` nullable'') ([] :*: Const False) g
  where deriv' :: Combinator (Derivative v) ~> Combinator (Derivative v)
        deriv' = liftRec deriv''
        deriv'' :: ParserF (Rec ParserF (Derivative v)) ~> ParserF (Rec ParserF (Derivative v))
        deriv'' p = case p of
          Cat a b -> Alt (deriv' a `cat` b) (delta a `cat` deriv' b)
          Alt a b -> Alt (deriv' a) (deriv' b)
          Rep p -> Map (uncurry (:)) (deriv' p `cat` many p)
          Map f p -> Map f (deriv' p)
          Bnd p f -> Bnd (deriv' p) f
          Lit c' -> if c == c' then Ret [c] else Nul
          Lab p s -> Lab (deriv' p) s
          _ -> Nul
        nullable :: Rec ParserF (Derivative v) a -> Bool
        nullable c = nullable' (fst (hisomap (hsnd . hsnd) (error "this path should not be traversed")) c)
        parseNull :: Rec ParserF (Derivative v) a -> [a]
        parseNull c = parseNull' (fst (hisomap (hfst . hsnd) (error "this path should not be traversed")) c)
        delta :: Combinator (Derivative v) ~> Combinator (Derivative v)
        delta c = if nullable c
          then ret (parseNull c)
          else nul

parseNull :: Parser a -> [a]
parseNull = parseNull' . unGraph

parseNull' :: Rec ParserF [] a -> [a]
parseNull' = rfold parseNull'' []

parseNull'' :: ParserF [] a -> [a]
parseNull'' parser = case parser of
  Cat a b -> (,) <$> a <*> b
  Alt a b -> a <> b
  Rep _ -> [[]]
  Map f p -> f <$> p
  Bnd p f -> p >>= f
  Ret as -> as
  Lab p _ -> p
  _ -> []

compact :: Parser a -> Parser a
compact = modifyGraph (graphMap compact'')
  where compact'' :: ParserF (Rec ParserF v) a -> ParserF (Rec ParserF v) a
        compact'' parser = case parser of
          Cat (In Nul) _ -> Nul
          Cat _ (In Nul) -> Nul
          Cat (In (Ret [t])) b -> Map ((,) t) b
          Cat a (In (Ret [t])) -> Map (flip (,) t) a
          Alt (In Nul) (In p) -> p
          Alt (In p) (In Nul) -> p
          Map f (In (Ret as)) -> Ret (f <$> as)
          Map g (In (Map f p)) -> Map (g . f) p
          Rep (In Nul) -> Ret []
          a -> a

nullable :: Parser a -> Bool
nullable = nullable' . unGraph

nullable' :: Rec ParserF (Const Bool) a -> Bool
nullable' = getConst  . rfold nullable'' (Const False)

nullable'' :: ParserF (Const Bool) a -> Const Bool a
nullable'' rec = case rec of
  Cat a b -> Const $ getConst a && getConst b
  Alt a b -> Const $ getConst a || getConst b
  Rep _ -> Const True
  Eps -> Const True
  _ -> Const False

size :: Parser a -> Int
size = getSum . getConst . fold (Const . mappend (Sum 1) . hfoldMap getConst) (Const (Sum 0))


-- Instances

instance HFunctor ParserF where
  hfmap f p = case p of
    Cat a b -> Cat (f a) (f b)
    Alt a b -> Alt (f a) (f b)
    Rep p -> Rep (f p)
    Map g p -> Map g (f p)
    Bnd p g -> Bnd (f p) (f . g)
    Lit c -> Lit c
    Ret as -> Ret as
    Nul -> Nul
    Eps -> Eps
    Lab p s -> Lab (f p) s

instance HFoldable ParserF where
  hfoldMap f p = case p of
    Cat a b -> f a <> f b
    Alt a b -> f a <> f b
    Rep p -> f p
    Map _ p -> f p
    Bnd p _ -> f p
    Lab p _ -> f p
    _ -> mempty

instance Functor (ParserF (Rec ParserF v)) where
  fmap = (. In) . Map

instance Functor (Rec ParserF v) where
  fmap = (In .) . Map

instance Functor (Graph ParserF) where
  fmap f (Graph rec) = Graph (f <$> rec)

instance Applicative (ParserF (Rec ParserF v)) where
  pure = Ret . pure
  fs <*> as = uncurry ($) <$> (In fs `Cat` In as)

instance Applicative (Rec ParserF v) where
  pure = In . Ret . pure
  (<*>) = (fmap (uncurry ($)) .) . (In .) . Cat

instance Applicative (Graph ParserF) where
  pure a = Graph (pure a)
  Graph f <*> Graph a = Graph (f <*> a)

instance Alternative (ParserF (Rec ParserF v)) where
  empty = Nul
  a <|> b = Alt (In a) (In b)
  some v = (:) <$> v <*> many v
  many p = Rep (In p)

instance Alternative (Rec ParserF v) where
  empty = In Nul
  (<|>) = (In .) . Alt
  some v = (:) <$> v <*> many v
  many = In . Rep

instance Alternative (Graph ParserF) where
  empty = Graph empty
  Graph a <|> Graph b = Graph (a <|> b)
  some (Graph p) = Graph (some p)
  many (Graph p) = Graph (many p)

instance Monad (Rec ParserF v) where
  return = pure
  (>>=) = (In .) . Bnd

instance Monad (Graph ParserF) where
  return = pure
  Graph p >>= f = Graph (p >>= unGraph . f)

instance HEqF ParserF
  where heqF eq a b = case (a, b) of
          (Cat a1 b1, Cat a2 b2) -> eq a1 a2 && eq b1 b2
          (Alt a1 b1, Alt a2 b2) -> eq a1 a2 && eq b1 b2
          (Rep p1, Rep p2) -> eq p1 p2
          -- (Map f1 p1, Map f2 p2) -> eq p1 p2
          -- (Bnd p1 f1, Bnd p2 f2) -> eq p1 p2
          (Lit c1, Lit c2) -> c1 == c2
          (Ret r1, Ret r2) -> length r1 == length r2
          (Nul, Nul) -> True
          (Eps, Eps) -> True
          (Lab p1 s1, Lab p2 s2) -> s1 == s2 && eq p1 p2
          _ -> False

instance HShowF ParserF
  where hshowsPrecF n showsPrec p = case p of
          Cat a b -> showsPrec n a . showString " `cat` " . showsPrec n b
          Alt a b -> showsPrec n a . showString " <|> " . showsPrec n b
          Rep p -> showString "many " . showsPrec n p
          Map _ p -> showString "f <$> " . showsPrec n p
          Bnd p _ -> showsPrec n p . showString " >>= f"
          Lit c -> showString "lit " . shows c
          Ret _ -> showString "ret […]"
          Nul -> showString "nul"
          Eps -> showString "eps"
          Lab p s -> showsPrec 2 p . showString " `label` " . shows s
