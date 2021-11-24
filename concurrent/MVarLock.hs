-- | This module demonstrates how to make an update 'action' happens only once.
-- MVar is used to lock the update action to a single thread
-- TVar is used to count the update action attempts (and make the first one fails)
--
-- Try with:
--   $ nix-shell -p "haskellPackages.ghcWithPackages(p: [p.stm p.say])"
--   [nix-shell] $ runhaskell MVarLock.hs
--
-- Other MVar example usage at https://hackage.haskell.org/package/mvar-lock-0.1.0.2/docs/src/Control.Concurrent.MVarLock.html
module MVarLock where

import Control.Concurrent (forkIO, myThreadId, threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, stateTVar)
import Control.Monad
import Say (sayString)

-------------------------------------------
-- MVar usage
-------------------------------------------
data Status = New | Ready

type UpdateLock = MVar Status

mkLock :: IO UpdateLock
mkLock = newMVar New

-- | An helper function to run an action only once
runUpdate :: UpdateLock -> IO () -> IO ()
runUpdate lock io = modifyMVar_ lock $ \status -> run status >> pure Ready
  where
    -- the update already happened
    run Ready = trace "already updated"
    -- otherwise run the io action
    run New = io

-------------------------------------------
-- TVar usage
-------------------------------------------
type Counter = TVar Int

mkCounter :: Int -> IO Counter
mkCounter = newTVarIO

-- | An helper function to decrement a counter atomically
decrCounter :: Counter -> STM Int
decrCounter counter = stateTVar counter $ \current -> (current, current - 1)

-------------------------------------------
-- Simulation playground
-------------------------------------------
main :: IO ()
main = do
  lock <- mkLock
  action <- mkAction
  fork10 $ runUpdate lock action
  -- wait 1 second to let thread do their things
  threadDelay 1000000
  where
    -- a fake update action, the first two will fail
    mkAction = do
      counter <- mkCounter 2
      pure $ do
        trace "updating..."
        -- here it is important to use an STM to prevent two threads from updating the same value
        count <- atomically $ decrCounter counter
        when (count > 0) $ error "too early"
        threadDelay 500000

-- | An helper function to log the current thread id
trace :: String -> IO ()
trace msg = do
  th <- myThreadId
  sayString $ show th <> ": " <> msg

-- | An helper function to start 10 thread for the given action
fork10 :: IO () -> IO ()
fork10 = replicateM_ 10 . forkIO
