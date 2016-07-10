{-# LANGUAGE PolyKinds #-}
module Control.Higher.Monad.Free where

data FreeF f v b a
  = Impure (f v b a)
  | Pure (v a)

newtype Free f v a = Free { runFree :: FreeF f v (Free f v) a }
