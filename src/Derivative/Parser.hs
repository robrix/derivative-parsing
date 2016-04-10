{-# LANGUAGE FlexibleInstances, GADTs, RankNTypes #-}
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
, Data.Higher.Graph.HRec(..)
, Data.Higher.Graph.HGraph(..)
) where

import Control.Applicative
import Data.Higher.Foldable
import Data.Higher.Functor
import Data.Higher.Functor.Eq
import Data.Higher.Functor.Show
import Data.Higher.Graph
import Data.Higher.Isofunctor
import qualified Data.Monoid as Monoid
import Data.Monoid hiding (Alt)

-- API

parse :: Parser a -> String -> [a]
parse p = parseNull . foldl ((compact .) . deriv) p


commaSep1 :: Parser a -> Parser [a]
commaSep1 = sep1 (HDown (lit ','))

commaSep :: Parser a -> Parser [a]
commaSep = sep (HDown (lit ','))

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
mu f = HDown $ Mu $ \ ~(v:_) -> pure $
  case f (Var v) of
    In r -> r
    p -> p `Lab` ""


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

type Parser a = HGraph ParserF a
type Combinator v a = HRec ParserF v a


-- Algorithm

deriv :: Parser a -> Char -> Parser a
deriv g c = modifyGraph deriv' g
  where deriv' :: HRec ParserF v a -> HRec ParserF v a
        deriv' (Var v) = Var v
        deriv' (Mu g) = Mu (map deriv'' . g)
        deriv' (In r) = In (deriv'' r)
        deriv'' :: ParserF (HRec ParserF v) a -> ParserF (HRec ParserF v) a
        deriv'' parser = case parser of
          Cat a b -> Alt (deriv' a `cat` b) (ret (parseNull (HDown (hisomap (const undefined) (const undefined) a))) `cat` deriv' b)
          Alt a b -> Alt (deriv' a) (deriv' b)
          Rep p -> Map (uncurry (:)) $ deriv' p `cat` many p
          Map f p -> Map f (deriv' p)
          Bnd p f -> Bnd (deriv' p) f
          Lit c' -> if c == c' then Ret [c] else Nul
          Lab p s -> Lab (deriv' p) s
          _ -> Nul

parseNull :: Parser a -> [a]
parseNull = parseNull' . hup

parseNull' :: HRec ParserF [] a -> [a]
parseNull' = hrfold go []
  where go :: ParserF [] a -> [a]
        go parser = case parser of
          Cat a b -> (,) <$> a <*> b
          Alt a b -> a <> b
          Rep _ -> [[]]
          Map f p -> f <$> p
          Bnd p f -> p >>= f
          Ret as -> as
          Lab p _ -> p
          _ -> []

compact :: Parser a -> Parser a
compact = modifyGraph (hmap compact'')
  where compact'' :: ParserF (HRec ParserF v) a -> ParserF (HRec ParserF v) a
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
nullable = nullable' . hup

nullable' :: HRec ParserF (Const Bool) a -> Bool
nullable' = rfold go False
  where go :: ParserF (Const Bool) b -> Bool
        go rec = case rec of
          Cat a b -> getConst a && getConst b
          Alt a b -> getConst a || getConst b
          Rep _ -> True
          Eps -> True
          _ -> False

size :: Parser a -> Int
size = getSum . fold (mappend (Sum 1) . hfoldMap getConst) (Sum 0)


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

instance Functor (ParserF (HRec ParserF v)) where
  fmap = (. In) . Map

instance Functor (HRec ParserF v) where
  fmap = (In .) . Map

instance Functor (HGraph ParserF) where
  fmap f (HDown rec) = HDown (f <$> rec)

instance Applicative (ParserF (HRec ParserF v)) where
  pure = Ret . pure
  fs <*> as = uncurry ($) <$> (In fs `Cat` In as)

instance Applicative (HRec ParserF v) where
  pure = In . Ret . pure
  (<*>) = (fmap (uncurry ($)) .) . (In .) . Cat

instance Applicative (HGraph ParserF) where
  pure a = HDown (pure a)
  HDown f <*> HDown a = HDown (f <*> a)

instance Alternative (ParserF (HRec ParserF v)) where
  empty = Nul
  a <|> b = Alt (In a) (In b)
  some v = (:) <$> v <*> many v
  many p = Rep (In p)

instance Alternative (HRec ParserF v) where
  empty = In Nul
  (<|>) = (In .) . Alt
  some v = (:) <$> v <*> many v
  many = In . Rep

instance Alternative (HGraph ParserF) where
  empty = HDown empty
  HDown a <|> HDown b = HDown (a <|> b)
  some (HDown p) = HDown (some p)
  many (HDown p) = HDown (many p)

instance Monad (HRec ParserF v) where
  return = pure
  (>>=) = (In .) . Bnd

instance Monad (HGraph ParserF) where
  return = pure
  HDown p >>= f = HDown (p >>= hup . f)

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
