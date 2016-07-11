{-# LANGUAGE RankNTypes, TypeOperators #-}
module Control.Higher.Monad where

import Control.Higher.Applicative
import Data.Higher.Pointed

class (HBind m, HPointed m) => HMonad m

class HApply m => HBind m where
  hbind :: m a p -> (a p -> m b q) -> m b q
