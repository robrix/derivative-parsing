{-# LANGUAGE PolyKinds, RankNTypes, TypeOperators #-}
module Control.Higher.Monad where

import Control.Higher.Applicative
import Data.Higher.Pointed
import Data.Higher.Transformation

class (HBind m, HPointed m) => HMonad m

class HApply m => HBind m where
  infixl 1 `hbind`
  hbind :: m a p -> (a ~> m b) -> m b p
