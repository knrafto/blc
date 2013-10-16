-- | Testing infinite values.
module Infinite
    ( isFinite
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Data.Maybe
import System.IO.Unsafe

-- | @'isBottom' a@ returns 'True' if @a@ is determined to be an infinite
-- value. An infinite value is considered to be one that does not complete
-- in 0.1 seconds, or throws 'NonTermination' or 'StackOverflow'.
isFinite :: a -> Bool
isFinite = fromMaybe False . unsafePerformIO . timeOut 100 . isComputable

-- | @'timeOut' t action@ returns @'Just' a@ if action evaluates to @a@ in
-- under @t@ milliseconds, and returns 'Nothing' if the computation timed
-- out.
timeOut :: Int -> IO a -> IO (Maybe a)
timeOut t action = do
    v <- newEmptyMVar
    runner <- forkIO $ action >>= putMVar v . Just
    reaper <- forkIO $ threadDelay t >> putMVar v Nothing
    result <- takeMVar v
    killThread runner
    killThread reaper
    return result

-- | Return 'True' if the value yields a result or fails with 'NonTemination'
-- or 'StackOverflow'.
isComputable :: a -> IO Bool
isComputable a = (True <$ evaluate a) `catches`
    [ Handler (\NonTermination -> return False)
    , Handler handleAsync
    ]
  where
    handleAsync StackOverflow = return False
    handleAsync e             = throwIO e
