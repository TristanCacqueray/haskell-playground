{-# LANGUAGE OverloadedStrings #-}

-- | Using statistics-linreg to find a given bug date
module Main
  ( main,
  )
where

import Control.Exception (catch)
import Data.Maybe (fromJust, isJust)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import qualified Data.Vector as V
import Statistics.LinearRegression (linearRegression)
import System.Environment (getArgs)
import Web.Bugzilla hiding (bugId)

getTime :: BugzillaSession -> BugId -> IO (Maybe UTCTime)
getTime session bugId = catch go ignoreException
  where
    go :: IO (Maybe UTCTime)
    go = do
      bug <- getBug session bugId
      return $ case bug of
        Just bug' -> Just $ bugCreationTime bug'
        Nothing -> Nothing
    ignoreException :: BugzillaException -> IO (Maybe UTCTime)
    ignoreException _ = return Nothing

getTimes :: [BugId] -> IO [Maybe UTCTime]
getTimes bugs = do
  session <- anonymousSession <$> newBugzillaContext "bugzilla.redhat.com"
  mapM (getTime session) bugs

filterBadBugs :: [BugId] -> [Maybe UTCTime] -> ([BugId], [UTCTime])
filterBadBugs bugs times = (goodBugs, map fromJust goodTimes)
  where
    (goodBugs, goodTimes) = unzip $ filter (isJust . snd) $ zip bugs times

-- >>> let bugs = [0, 100]
-- >>> now <- getCurrentTime
-- >>> let yesterday = addUTCTime (-3600*24) now
-- >>> let dates = [yesterday, now]
-- >>> predictTime' bugs dates 200
-- tomorrow -- in utc time
predictTime' :: [BugId] -> [UTCTime] -> BugId -> UTCTime
predictTime' bugs times target = posixSecondsToUTCTime $ predict $ linearRegression xs ys
  where
    predict :: (Double, Double) -> POSIXTime
    predict (alpha, beta) = realToFrac ((fromIntegral target - alpha) / beta)
    ys = V.fromList $ map fromIntegral bugs
    xs = V.fromList $ map (realToFrac . utcTimeToPOSIXSeconds) times

predictTime :: BugId -> Int -> BugId -> IO UTCTime
predictTime highestBug sampleSize target =
  do
    sampleTimes <- getTimes sampleBugs
    let (bugs, times) = filterBadBugs sampleBugs sampleTimes
    print $ "Sampling with " <> show (length bugs) <> " bugs from " <> show (last bugs) <> " to " <> show (head bugs)
    return $ predictTime' bugs times target
  where
    sampleBugs = take sampleSize $ getRange highestBug 1
    -- Look for bugId spread apart
    getRange cur step
      | cur < 0 = []
      | otherwise = cur : getRange (cur - step) (step + 20)

main :: IO ()
main = do
  args <- getArgs
  case fmap read args of
    [highestBug, sampleSize, target] -> do
      prediction <- predictTime highestBug sampleSize target
      print prediction
    _ -> putStrLn "usage: bugzilla-demo highest-bug sample-size target-bug"
