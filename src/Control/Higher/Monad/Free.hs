{-# LANGUAGE PolyKinds #-}
module Control.Higher.Monad.Free where

data FreeF f v b a
  = Impure (f b a)
  | Pure (v a)

newtype Free f v a = Free { runFree :: FreeF f v (Free f v) a }


wrap :: f (Free f v) a -> Free f v a
wrap = Free . Impure
