{-# LANGUAGE RankNTypes, TypeOperators #-}
module Control.Higher.Monad where

import Control.Higher.Applicative

class HApplicative m => HMonad m where
  hbind :: m a p -> (a p -> m b q) -> m b q
