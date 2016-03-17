{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Data.Higher.Function where

hfix :: forall f g a. (forall a. (forall a. f a -> g a) -> f a -> g a) -> f a -> g a
hfix f = x
  where x :: forall a. f a -> g a
        x = f x
