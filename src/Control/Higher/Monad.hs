{-# LANGUAGE RankNTypes, TypeOperators #-}
module Control.Higher.Monad where

import Control.Higher.Applicative
import Data.Higher.Transformation

class HApplicative m => HMonad m where
  hbind :: m a z -> (a ~> m b) -> m b z
