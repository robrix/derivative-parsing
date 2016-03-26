{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Data.Memo
( memo
, memoFrom
, memoStable
, memoStableFrom
) where

import Data.IORef
import System.IO.Unsafe
import System.Mem.StableName

applyStable :: IORef [(StableName a, b)] -> Maybe b -> (a -> b) -> a -> b
applyStable ref from f arg = unsafePerformIO $ do
  name <- makeStableName arg
  table <- readIORef ref
  case name `lookup` table of
    Just value -> return value
    _ -> do
      _ <- case from of
        Just from -> modifyIORef' ref (insert name from)
        _ -> return ()
      let result = f arg
      result `seq` modifyIORef' ref (insert name result)
      return $! result

apply :: Eq a => IORef [(a, b)] -> Maybe b -> (a -> b) -> a -> b
apply ref from f arg = unsafePerformIO $ do
  table <- readIORef ref
  case arg `lookup` table of
    Just result -> return result
    _ -> do
      _ <- case from of
        Just from -> modifyIORef' ref (insert arg from)
        _ -> return ()
      let result = f arg
      result `seq` modifyIORef' ref (insert arg result)
      return $! result

memoStable :: (a -> b) -> a -> b
memoStable f = unsafePerformIO $ do
  ref <- newIORef []
  ref `seq` return $! applyStable ref Nothing f

memoStableFrom :: b -> (a -> b) -> a -> b
memoStableFrom from f = unsafePerformIO $ do
  ref <- newIORef []
  ref `seq` return $! applyStable ref (Just from) f

memo :: Eq a => (a -> b) -> a -> b
memo = memoWith Nothing

memoFrom :: Eq a => b -> (a -> b) -> a -> b
memoFrom from = memoWith (Just from)

memoWith :: Eq a => Maybe b -> (a -> b) -> a -> b
memoWith from f = unsafePerformIO $ do
  ref <- newIORef []
  ref `seq` return $! apply ref from f

insert :: key -> value -> [(key, value)] -> [(key, value)]
insert key value = ((key, value) :)
