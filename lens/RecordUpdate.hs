-- RankNTypes is needed to define the simpler view' over'
{-# LANGUAGE RankNTypes #-}

-- | The goal is to replace the `cralwers_api_key` value for a given workspace.
-- It used to be the actual key, but it is now the name of an environment variable name.
-- Thus we need a way to patch workspace
module RecordUpdate where

import Control.Lens hiding (Index)
import System.Environment (getEnv)

data Workspace = Workspace
  { name :: String,
    crawlers_api_key :: String,
    users :: [String],
    count :: Int
  }
  deriving (Show)

setSecretManually :: Workspace -> String -> Workspace
setSecretManually workspace newSecret =
  Workspace (name workspace) newSecret (users workspace) (count workspace)

{-
      +-- Bigger type
      |
      v
Lens' bigger smaller
             ^
             |
             +--  Smaller type within the bigger type
-}

view' :: Lens' a b -> a -> b
view' = view

over' :: Lens' a b -> (b -> b) -> a -> a
over' = over

apiKeyLens :: Lens' Workspace String
apiKeyLens = lens crawlers_api_key (\workspace newKey -> workspace {crawlers_api_key = newKey})

countLens :: Lens' Workspace Int
countLens = lens count (\workspace newCount -> workspace {count = newCount})

-- using lens: map (over apiKeyLens (const "api-key")) config

data Index = Index
  { index :: String,
    workspace :: Workspace
  }
  deriving (Show)

changeApiKeyManually :: Index -> String -> Index
changeApiKeyManually idx newSecret = idx {workspace = (workspace idx) {crawlers_api_key = newSecret}}

wsLens :: Lens' Index Workspace
wsLens = lens workspace (\index newWs -> index {workspace = newWs})

-- using lens: over' (wsLens . countLens) (+1) superConf

config :: [Workspace]
config =
  [ Workspace "demo1" "CRAWLERS_API_KEY" [] 0,
    Workspace "demo2" "CRAWLERS_API_KEY" [] 2
  ]

superConf :: Index
superConf = Index "index" (head config)

main :: IO ()
main = print =<< mapMOf (wsLens . apiKeyLens) getEnv superConf
