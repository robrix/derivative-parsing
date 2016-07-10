{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, RankNTypes, TypeOperators #-}
module Data.Higher.Foldable where

import Control.Applicative hiding (Const(..))
import Data.Bifunctor
import Data.Function
import Data.Higher.Transformation
import Data.Monoid

class HFoldable f where
  hfoldMap :: (Monad c, Alternative m) => (c ~> m) -> f c ~> m

  hfold :: (Monad m, Alternative m) => f m ~> m
  hfold = hfoldMap id

  hlength :: Monad c => f c a -> Int
  hlength = getSum . getConst . hfoldMap (const (Const (Sum 1)))


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
