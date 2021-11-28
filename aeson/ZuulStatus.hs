#!/usr/bin/env stack
{- stack
    --resolver lts-16.3
    script
    --package aeson
    --package bytestring
    --package req
    --package text
 -}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- ZuulStatus demonstrate aeson generic from json decoder

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Maybe
import Data.Text (Text (..))
import qualified Data.Text as T
import GHC.Generics
import Network.HTTP.Req

data Job
  = Job
      { name :: Text,
        uuid :: Maybe Text,
        result :: Maybe Text
      }
  deriving (Show, Generic, FromJSON)

data Change
  = Change
      { id :: Maybe Text,
        ref :: Text,
        project :: Text,
        live :: Bool,
        active :: Bool,
        jobs :: [Job]
      }
  deriving (Show, Generic, FromJSON)

data Changes = Changes [Change]
  deriving (Show, Generic, FromJSON)

data ChangeQueue
  = ChangeQueue
      { name :: Text,
        heads :: [Changes]
      }
  deriving (Show, Generic, FromJSON)

data Pipeline
  = Pipeline
      { name :: Text,
        change_queues :: [ChangeQueue]
      }
  deriving (Show, Generic, FromJSON)

data Status
  = Status
      { zuul_version :: Text,
        pipelines :: [Pipeline]
      }
  deriving (Show, Generic, FromJSON)

liveChanges :: Status -> [Change]
liveChanges status = concat $ map processPipeline (pipelines status)
  where
    processPipeline :: Pipeline -> [Change]
    processPipeline Pipeline {name, change_queues} = concat $ map processQueue change_queues
    processQueue :: ChangeQueue -> [Change]
    processQueue ChangeQueue {name, heads} = concat $ map processChanges heads
    processChanges :: Changes -> [Change]
    processChanges (Changes changes) = filter (\c -> live c && active c) changes

jobUuid :: [Change] -> [Text]
jobUuid changes = concat $ map go changes
  where
    go :: Change -> [Text]
    go Change {..} = getUuids' jobs
    getUuids' :: [Job] -> [Text]
    getUuids' jobs = do
      job <- jobs
      guard $ isJust (uuid job)
      return $ fromJust $ uuid job
    getUuids :: [Job] -> [Text]
    getUuids [] = []
    getUuids (x : xs) = case uuid x of
      Just u -> u : getUuids xs
      Nothing -> getUuids xs

process :: Status -> IO ()
process status = print $ jobUuid $ liveChanges status

zuulStatusReq ::
  (MonadHttp m, FromJSON a) => T.Text -> Int -> m (JsonResponse a)
zuulStatusReq host port' =
  req
    GET
    (http host /: "zuul" /: "api" /: "status")
    NoReqBody
    jsonResponse
    (port port')

main :: IO ()
main = runReq defaultHttpConfig $ do
  r <- zuulStatusReq "localhost" 8080
  liftIO $ process (responseBody r)
