{-# LANGUAGE GADTs #-}
module Derivative.Parser
( commaSep
, cat
, commaSep1
, deriv
, literal
, oneOf
, parse
, parseNull
, Parser(..)
, sep
, sep1
) where

import Control.Applicative

-- API

parse :: Parser a -> String -> [a]
parse p = parseNull . foldl ((compact .) . deriv) p


cat :: Parser a -> Parser b -> Parser (a, b)
cat = Cat

literal :: String -> Parser String
literal string = sequenceA (Lit <$> string)

commaSep1 :: Parser a -> Parser [a]
commaSep1 = sep1 (Lit ',')

commaSep :: Parser a -> Parser [a]
commaSep = sep (Lit ',')

sep1 :: Parser sep -> Parser a -> Parser [a]
sep1 s p = (:) <$> p <*> many (s *> p)

sep :: Parser sep -> Parser a -> Parser [a]
sep s p = s `sep1` p <|> pure []

oneOf :: (Foldable t, Alternative f) => t a -> f a
oneOf = foldr ((<|>) . pure) empty


-- Types

-- | A parser type encoding concatenation, alternation, repetition, &c. as first-order constructors.
data Parser a where
  Cat :: Parser a -> Parser b -> Parser (a, b)
  Alt :: Parser a -> Parser b -> Parser (Either a b)
  Rep :: Parser a -> Parser [a]
  Map :: (a -> b) -> Parser a -> Parser b
  Bnd :: Parser a -> (a -> Parser b) -> Parser b
  Lit :: Char -> Parser Char
  Ret :: [a] -> Parser a
  Nul :: Parser a
  Eps :: Parser a


-- Algorithm

deriv :: Parser a -> Char -> Parser a
deriv (Cat a b) c = Cat (deriv a c) b <|> Cat (Ret (parseNull a)) (deriv b c)
deriv (Alt a b) c = Alt (deriv a c) (deriv b c)
deriv (Rep p) c = (:) <$> deriv p c <*> Rep p
deriv (Map f p) c = Map f (deriv p c)
deriv (Bnd p f) c = Bnd (deriv p c) f
deriv (Lit c') c = if c == c' then Ret [c] else Nul
deriv _ _ = Nul

parseNull :: Parser a -> [a]
parseNull (Cat a b) = (,) <$> parseNull a <*> parseNull b
parseNull (Alt a b) = (Left <$> parseNull a) ++ (Right <$> parseNull b)
parseNull (Rep _) = [[]]
parseNull (Map f p) = f <$> parseNull p
parseNull (Bnd p f) = (f <$> parseNull p) >>= parseNull
parseNull (Ret as) = as
parseNull _ = []

compact :: Parser a -> Parser a
compact (Cat Nul _) = Nul
compact (Cat _ Nul) = Nul
compact (Cat (Ret [t]) b) = (,) t <$> b
compact (Cat a (Ret [t])) = flip (,) t <$> a
compact (Alt Nul p) = Right <$> p
compact (Alt p Nul) = Left <$> p
compact (Map f (Ret as)) = Ret (f <$> as)
compact (Map g (Map f p)) = g . f <$> p
compact (Rep Nul) = Ret []
compact a = a


-- Instances

instance Functor Parser where
  fmap = Map

instance Applicative Parser where
  pure = Ret . pure
  (<*>) = (fmap (uncurry ($)) .) . Cat

instance Alternative Parser where
  empty = Nul
  (<|>) = (fmap (either id id) .) . Alt
  some v = (:) <$> v <*> many v
  many = Rep

instance Monad Parser where
  return = pure
  (>>=) = Bnd
