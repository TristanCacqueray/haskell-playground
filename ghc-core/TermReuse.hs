-- |
module Main where

import Debug.Trace (trace)

data User = User {trTerm :: String} deriving (Show)

getOld :: IO [User]
getOld = pure $ User <$> ["alice", "bob"]

getRecent :: IO [User]
getRecent = pure $ User <$> ["bob", "eve"]

getNew :: IO [User]
getNew = do
  beforeAuthor <- getOld
  afterAuthor <- getRecent

  let ba = trace "Getting ba for real" $ trTerm <$> beforeAuthor
  -- pure $ filter (\tr -> trTerm tr `notElem` ba) afterAuthor

  let keepAuthor tr = trTerm tr `notElem` (trace "Getting ba" $ ba) -- trTerm <$> beforeAuthor)
  pure $ filter keepAuthor afterAuthor

main = print =<< getNew
