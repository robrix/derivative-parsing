{-# LANGUAGE FlexibleInstances, GADTs, RankNTypes #-}
module Derivative.Parser
( cat
, commaSep
, commaSep1
, compact
, deriv
, label
, lit
, literal
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
, nullable
) where

import Control.Applicative
import Data.Bifunctor (first)
import Data.Higher.Foldable
import Data.Higher.Functor
import Data.Higher.Functor.Eq
import Data.Higher.Functor.Show
import Data.Higher.Graph
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

infixl 4 `cat`

cat :: Combinator v a -> Combinator v b -> Combinator v (a, b)
cat (In Nul) _ = In Nul
cat (In (Ret [t])) b = (,) t <$> b
cat (In (Cat a b)) c = (\ (a, (b, c)) -> ((a, b), c)) <$> cat a (cat b c)
cat (In (Map f a)) b = first f <$> cat a b
cat a b = In (Cat a b)

lit :: Char -> Combinator v Char
lit = In . Lit

delta :: Combinator v a -> Combinator v a
delta (In Nul) = In Nul
delta (In (Ret a)) = In (Ret a)
delta (In (Del p)) = In (Del p)
delta a = In (Del a)

ret :: [a] -> Combinator v a
ret = In . Ret

infixr 2 `label`

label :: Combinator v a -> String -> Combinator v a
label p s = In $ case p of In Nul -> Nul
                           In (Ret t) -> Ret t
                           In (Del p) -> Del p
                           _      -> Lab p s

literal :: String -> Combinator v String
literal string = sequenceA (In . Lit <$> string)

mu :: (forall v. Combinator v a -> Combinator v a) -> Parser a
mu f = Graph $ Mu $ \ v -> case f (Var v) of
  In r -> r
  p -> p `Lab` ""

parser :: (forall v. Combinator v a) -> Parser a
parser r = compact $ Graph r

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
  Lab :: f a -> String -> ParserF f a
  Del :: f a -> ParserF f a

type Parser = Graph ParserF
type Combinator v = Rec ParserF v


-- Algorithm

deriv :: Parser a -> Char -> Parser a
deriv g c = modifyGraph deriv' g
  where deriv' :: Combinator v a -> Combinator v a
        deriv' = liftRec deriv''
        deriv'' :: ParserF (Combinator v) a -> ParserF (Combinator v) a
        deriv'' p = repack $ case p of
          Cat a b -> deriv' a `cat` b <|> delta a `cat` deriv' b
          Alt a b -> deriv' a <|> deriv' b
          Rep p -> uncurry (:) <$> (deriv' p `cat` many p)
          Map f p -> f <$> deriv' p
          Bnd p f -> deriv' p >>= f
          Lit c' -> if c == c' then pure c else empty
          Lab p s -> deriv' p `label` s
          _ -> empty
        repack (In r) = r
        repack a = Lab a ""

parseNull :: Parser a -> [a]
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

compact :: Parser a -> Parser a
compact = transform compact''

compact' :: Combinator v a -> Combinator v a
compact' = liftRec compact''

compact'' :: ParserF (Combinator v) a -> ParserF (Combinator v) a
compact'' parser = case parser of
  Cat (In Nul) _ -> Nul
  Cat _ (In Nul) -> Nul
  Cat (In (Ret [t])) b -> Map ((,) t) b
  Cat a (In (Ret [t])) -> Map (flip (,) t) a
  Cat (In (Cat a b)) c -> Map (\ (a, (b, c)) -> ((a, b), c)) (cat a (cat b c))
  Cat (In (Map f a)) b -> Map (first f) (cat a b)
  Alt (In Nul) (In p) -> p
  Alt (In p) (In Nul) -> p
  Alt (In (Ret a)) (In (Ret b)) -> Ret (a <> b)
  Map f (In (Ret as)) -> Ret (f <$> as)
  Map g (In (Map f p)) -> Map (g . f) p
  Map _ (In Nul) -> Nul
  Rep (In Nul) -> Ret [[]]
  Lab (In Nul) _ -> Nul
  Lab (In (Ret t)) _ -> Ret t
  Lab (In (Del p)) _ -> Del p
  Del (In Nul) -> Nul
  Del (In (Del p)) -> Del p
  Del (In (Ret a)) -> Ret a
  a -> a

nullable :: Parser a -> Bool
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
    Lab p s -> Lab (f p) s
    Del a -> Del (f a)

instance HFoldable ParserF where
  hfoldMap f p = case p of
    Cat a b -> f a <> f b
    Alt a b -> f a <> f b
    Rep p -> f p
    Map _ p -> f p
    Bnd p _ -> f p
    Lab p _ -> f p
    _ -> mempty

instance Functor (Rec ParserF v) where
  fmap f p = In $ case p of
    In Nul -> Nul
    In (Map g p) -> Map (f . g) p
    In (Ret as) -> Ret (f <$> as)
    _ -> Map f p

instance Functor (Graph ParserF) where
  fmap f (Graph rec) = Graph (f <$> rec)

instance Applicative (Rec ParserF v) where
  pure = In . Ret . pure
  a <*> b = uncurry ($) <$> (a `cat` b)

instance Applicative (Graph ParserF) where
  pure a = Graph (pure a)
  Graph f <*> Graph a = Graph (f <*> a)

instance Alternative (Rec ParserF v) where
  empty = In Nul
  In Nul <|> b = b
  a <|> In Nul = a
  In (Ret a) <|> In (Ret b) = In (Ret (a <> b))
  a <|> b = In (Alt a b)
  some v = (:) <$> v <*> many v
  many (In Nul) = pure []
  many p = In (Rep p)

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
          (Lab p1 s1, Lab p2 s2) -> s1 == s2 && eq p1 p2
          _ -> False

instance HShowF ParserF
  where hshowsPrecF n showsPrec p = case p of
          Cat a b -> showParen (n > 4) $ showsPrec 4 a . showString " `cat` " . showsPrec 5 b
          Alt a b -> showParen (n > 3) $ showsPrec 3 a . showString " <|> " . showsPrec 4 b
          Rep p -> showParen (n >= 10) $ showString "many " . showsPrec 10 p
          Map _ p -> showParen (n > 4) $ showString "f <$> " . showsPrec 5 p
          Bnd p _ -> showParen (n > 1) $ showsPrec 1 p . showString " >>= f"
          Lit c -> showParen (n >= 10) $ showString "lit " . shows c
          Ret t -> showString "ret [" . showIndices (length t) . showString "]"
          Nul -> showString "empty"
          Lab p s -> showParen (n > 2) $ showsPrec 3 p . showString " `label` " . shows s
          Del a -> showParen (n >= 10) $ showString "δ " . showsPrec 10 a
          where showIndices n = foldr (.) id ((showChar 't' .) . shows <$> take n (iterate succ (0 :: Integer)))
