module Data.Memo
(
) where

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

insert :: key -> value -> Table key value -> [(key, value)]
insert key value = ((key, value) :)
