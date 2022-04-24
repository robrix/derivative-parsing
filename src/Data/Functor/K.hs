{-# LANGUAGE DeriveFunctor #-}
module Data.Functor.K (K(..)) where

import Control.Applicative

newtype K a b = K { getK :: a }
  deriving (Eq, Functor, Ord, Show)


-- Instances

instance Monoid a => Applicative (K a)
  where pure = const (K mempty)
        K a <*> K b = K (a <> b)

instance Monoid a => Alternative (K a)
  where empty = K mempty
        K a <|> K b = K (a <> b)

instance Monoid a => Monad (K a)
  where return = pure
        K a >>= _ = K a
