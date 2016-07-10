module Data.Higher.Functor.Identity where

newtype Identity f a = Identity { runIdentity :: f a }
