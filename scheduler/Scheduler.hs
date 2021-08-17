{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Monad
import Control.Scheduler
import Data.Text
import Data.Time.Clock
import Say

myLog txt = do
  th <- myThreadId
  say $ pack (show th) <> ": " <> txt

aTask :: Text -> Float -> IO Float
aTask name th = do
  myLog $ "begin: " <> name
  threadDelay (fromInteger $ round (th * 1_000_000))
  myLog $ " over: " <> name
  pure th

crawlers = [aTask "first" 1, aTask "second" 2, aTask "third" 0.5]

main = loop
  where
    loop = do
      start <- getCurrentTime
      traverseConcurrently_ (ParN 8) id crawlers
      end <- getCurrentTime
      let elapsed = diffUTCTime end start
      -- sleep loop_delay - elapsed
      -- if elapsed > loop_delay then print "Too slow!"
      threadDelay 5_000_000
      loop

main' = withScheduler_ (ParN 8) doSchedule
  where
    doSchedule sch = do
      scheduleWork sch (aTask "first" 1)
      scheduleWork sch (aTask "second" 2)
      scheduleWork sch (aTask "last" 0.5)
      threadDelay 1_000_000
      scheduleWork sch (aTask "first" 1)
      scheduleWork sch (aTask "second" 2)
      scheduleWork sch (aTask "last" 0.5)
      threadDelay 1_000_000
