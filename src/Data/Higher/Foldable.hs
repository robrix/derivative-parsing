{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, RankNTypes, TypeOperators #-}
module Data.Higher.Foldable where

import Control.Applicative hiding (Const(..))
import Data.Function
import Data.Higher.Transformation
import Data.Monoid

class HFoldable f where
  hfoldMap :: (Monad c, Alternative m) => (c ~> m) -> f c ~> m

  hfold :: (Monad m, Alternative m) => f m ~> m
  hfold = hfoldMap id


newtype Const a b = Const { getConst :: a }
  deriving (Eq, Functor, Ord, Show)

instance Monoid m => Applicative (Const m) where
  pure _ = empty
  Const a <*> Const b = Const (a <> b)

instance Monoid m => Alternative (Const m) where
  empty = Const mempty
  (<|>) = (Const .) . (mappend `on` getConst)

instance Monoid m => Monad (Const m) where
  return = pure
  Const m >>= _ = Const m
