-- |
module Main where

import Control.Concurrent (threadDelay)
-- From async-pool
import Control.Concurrent.Async.Pool

work :: Int -> IO ()
work x = do
  threadDelay 1000000
  print x

go :: TaskGroup -> IO ()
go tg = do
  _ <- mapConcurrently tg work (take 100 [1 ..])
  pure ()

main :: IO ()
main = withTaskGroup 4 go
