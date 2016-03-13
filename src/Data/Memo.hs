module Data.Memo
(
) where

import Control.Arrow
import Data.IORef
import System.IO.Unsafe

applyPartial :: Eq key => IORef [(key, value)] -> value -> ((Maybe key, input) -> value) -> (Maybe key, input) -> value
applyPartial ref from f arg | (Just key, _) <- arg = unsafePerformIO $ do
                              table <- readIORef ref
                              maybe (write ref key) return (key `lookup` table)
                            | otherwise = f arg
  where write ref key = do
          modifyIORef' ref (insert key from)
          let result = f arg
          result `seq` modifyIORef' ref (insert key result)
          return $! result

memoOn :: Eq key => (input -> Maybe key) -> value -> (input -> value) -> input -> value
memoOn on from = (. (on &&& id)) . memoPartial from . (. snd)

memoPartial :: Eq key => value -> ((Maybe key, input) -> value) -> (Maybe key, input) -> value
memoPartial from f = unsafePerformIO $ do
  ref <- newIORef []
  ref `seq` return $! applyPartial ref from f

insert :: key -> value -> Table key value -> [(key, value)]
insert key value = ((key, value) :)
