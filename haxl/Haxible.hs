-- | A experiment to try Haxl and implement a parallel ansible runner: Haxible
-- nix-shell -p "haskellPackages.ghcWithPackages(p: [p.haxl p.hashable p.text])"
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ApplicativeDo #-}

module Haxible where

import Control.Exception.Base (Exception)
import Haxl.Prelude
import Control.Monad hiding (mapM_)
import Prelude hiding (mapM_)
import Data.Hashable
import Data.List
import Data.Traversable (for)
import Data.Typeable
import Haxl.Core
import System.Random
import Data.Bool (bool)
import System.Directory (doesPathExist)
import Control.Concurrent (threadDelay)
import Data.Time.Clock

data TaskResult = Success | Failed | Skipped deriving (Show)

data FileResult = Absent | Present deriving (Show)

data CommandResult = ExitSuccess | ExitFailure Int deriving (Show)

newtype Command = Command String deriving (Eq, Show)

loggy :: String -> IO ()
loggy msg = do
  now <- getCurrentTime
  putStrLn $ show now <> ": [+] " <> msg

runFileTask :: FilePath -> IO FileResult
runFileTask fp = do
  -- threadDelay (10000 * length fp)
  loggy $ "file task: " <> fp
  bool Absent Present <$> doesPathExist fp

runCommandTask :: Command -> IO CommandResult
runCommandTask (Command c) = do
  threadDelay (10000 * length c)
  loggy $ "command: " <> c
  pure $ case c of
    "fail" -> ExitFailure 42
    _ -> ExitSuccess

fileTask :: FilePath -> Haxl FileResult
fileTask fp = dataFetch (FileTask fp)

commandTask :: Command -> Haxl ()
commandTask c = dataFetch (CommandTask c)
commandTask' = void . commandTask

creates :: Command -> FilePath -> Haxl ()
creates command fp = do
  exist <- fileTask fp
  case exist of
    Present -> pure ()
    Absent -> void $ commandTask command

main :: IO ()
main = do
  let stateStore = stateSet TaskState{} stateEmpty
  env0 <- initEnv stateStore ()
  results <- runHaxl env0 $ do
    setupZuul
    setupNodepool
    finalize
  print results

setupZuul :: Haxl ()
setupZuul = do
  Command "mkdir /etc/zuul" `creates` "/etc/zuul"
  Command "echo [default] > /etc/zuul/zuul.conf" `creates` "/etc/zuul/zuul.conf"

setupNodepool :: Haxl ()
setupNodepool = do
  Command "mkdir /etc/nodepool" `creates` "/etc/nodepool"
  Command "echo [] > /etc/nodepool/nodepool.yaml" `creates` "/etc/nodepool/nodepool.yaml"

finalize :: Haxl ()
finalize = commandTask' (Command "playbook over")

type Haxl = GenHaxl () ()

data TaskReq a where
  FileTask :: FilePath -> TaskReq FileResult
  CommandTask :: Command -> TaskReq ()
  deriving (Typeable)

deriving instance Eq (TaskReq a)
instance Hashable (TaskReq a) where
   hashWithSalt s (CommandTask (Command c)) = hashWithSalt s (0::Int, c)
   hashWithSalt s (FileTask a) = hashWithSalt s (1::Int, a)

deriving instance Show (TaskReq a)
instance ShowP TaskReq where showp = show

instance StateKey TaskReq where
  data State TaskReq = TaskState {}

instance DataSourceName TaskReq where
  dataSourceName _ = "TaskDataSource"

instance DataSource u TaskReq where
  fetch _state _flags _userEnv = SyncFetch $ \blockedFetches -> do
    loggy $ "Fetch starts"

    let
      fps :: [FilePath]
      fpsResults :: [ResultVar FileResult]
      (fps, fpsResults) = unzip
        [(fp, r) | BlockedFetch (FileTask fp) r <- blockedFetches]

      commands :: [Command]
      commandsResults :: [ResultVar ()]
      (commands, commandsResults) = unzip $ reverse $
        [(x, y) | BlockedFetch (CommandTask x) y <- blockedFetches]

    unless (null fps) $ do
      loggy $ "We queries these fps: " <> show fps
      res <- traverse runFileTask fps
      mapM_ (uncurry putSuccess) (zip fpsResults res)

    unless (null commands) $ do
      loggy $ "We run commands: " <> show commands
      res <- traverse runCommandTask commands
      let putRes (r, v) = case v of
            ExitSuccess -> putSuccess r ()
            ExitFailure code -> putFailure r (CommandException $ "command failure with: " <> show code)
      mapM_ putRes (zip commandsResults res)

    loggy $ "Fetch done\n"


newtype CommandException = CommandException String deriving (Show)
instance Exception CommandException
