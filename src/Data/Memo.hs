module Data.Memo
( memoOn
) where

import Control.Arrow
import Data.IORef
import System.IO.Unsafe

applyPartial :: Eq key => IORef [(key, value)] -> Maybe value -> ((Maybe key, input) -> value) -> (Maybe key, input) -> value
applyPartial ref from f arg | (Just key, _) <- arg = unsafePerformIO $ do
                              table <- readIORef ref
                              maybe (write ref key) return (key `lookup` table)
                            | otherwise = f arg
  where write ref key = do
          _ <- case from of
            Just from -> modifyIORef' ref (insert key from)
            _ -> return ()
          let result = f arg
          result `seq` modifyIORef' ref (insert key result)
          return $! result

memoOn :: Eq key => (input -> Maybe key) -> Maybe value -> (input -> value) -> input -> value
memoOn on from = (. (on &&& id)) . memoPartial from . (. snd)

memoPartial :: Eq key => Maybe value -> ((Maybe key, input) -> value) -> (Maybe key, input) -> value
memoPartial from f = unsafePerformIO $ do
  ref <- newIORef []
  ref `seq` return $! applyPartial ref from f

insert :: key -> value -> [(key, value)] -> [(key, value)]
insert key value = ((key, value) :)
