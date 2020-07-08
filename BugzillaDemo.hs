{-# LANGUAGE OverloadedStrings #-}

-- |
module BugzillaDemo where

import Control.Exception (catch)
import Data.Time.Clock
import Web.Bugzilla

getTime :: BugzillaSession -> BugId -> IO (Maybe UTCTime)
getTime session bugId = catch go ignoreException
  where
    go :: IO (Maybe UTCTime)
    go = do
      bug <- getBug session bugId
      return $ case bug of
        Just bug -> Just $ bugCreationTime bug
        Nothing -> Nothing
    ignoreException :: BugzillaException -> IO (Maybe UTCTime)
    ignoreException _ = return Nothing

getTimes :: [BugId] -> IO [Maybe UTCTime]
getTimes bugs = do
  session <- anonymousSession <$> newBugzillaContext "bugzilla.redhat.com"
  sequence $ map (getTime session) bugs
