{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
module Data.Functor.Const where

import Control.Applicative hiding (Const(..))
import Data.Bifunctor
import Data.Function
import Data.Monoid

newtype Const a b = Const { getConst :: a }
  deriving (Eq, Functor, Ord, Show)

instance Bifunctor Const where
  bimap f _ (Const a) = Const (f a)

instance Monoid m => Applicative (Const m) where
  pure _ = empty
  Const a <*> Const b = Const (a <> b)

instance Monoid m => Alternative (Const m) where
  empty = Const mempty
  (<|>) = (Const .) . (mappend `on` getConst)

instance Monoid m => Monad (Const m) where
  return = pure
  Const m >>= _ = Const m
