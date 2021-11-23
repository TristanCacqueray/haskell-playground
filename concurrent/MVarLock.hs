-- | This module demonstrates how to make an update 'action' happens only once.
module MVarLock where

import Control.Concurrent (myThreadId, threadDelay)
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar)
import Control.Monad (void)
import Say (sayString)

trace :: String -> IO ()
trace msg = do
  th <- myThreadId
  sayString $ show th <> ": " <> msg

-- | An helper function to run an action only once
runUpdate :: MVar Bool -> IO () -> IO ()
runUpdate updated io = modifyMVar_ updated go
  where
    -- the update already happened
    go True = trace "already updated" >> pure True
    -- otherwise run the io action
    go False = io >> pure True

main :: IO ()
main = do
  updated <- newMVar False
  void $ mapConcurrently (const $ runUpdate updated action) [1 .. 10]
  where
    action = do
      trace "updating..."
      threadDelay 500000
