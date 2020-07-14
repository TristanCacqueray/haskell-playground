#!/usr/bin/env stack
{- stack
    --resolver lts-16.3
    script
    --package directory
    --package bytestring
    --package text
    --package simple-cmd
 -}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless)
import Data.Aeson (FromJSON, parseJSON, decode, genericParseJSON)
import Data.Aeson.Casing (aesonPrefix, pascalCase)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Maybe (fromMaybe, fromJust, isNothing, isJust)
import Data.Text (Text)
import GHC.Generics (Generic)
import SimpleCmd (cmd, cmdMaybe, cmd_)
import System.Directory (doesDirectoryExist, doesFileExist)

-- Data to decode podman container inspection
data ContainerState
  = ContainerState
      { containerRunning :: Bool,
        containerStatus :: Text
      }
  deriving (Show, Generic)

instance FromJSON ContainerState where
  -- this custom decoder takes care of setting 'Status' to the `containerStatus` attribute
  -- because attribute name can't start with an uppercase, we can't use DeriveAnyClass
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

data Container
  = Container
      { containerId :: Text,
        containerState :: ContainerState
      }
  deriving (Show, Generic)

instance FromJSON Container where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

inspectContainer :: String -> IO (Maybe Container)
inspectContainer name = do
  podInspect <- pack . fromMaybe [] <$> cmdMaybe "podman" ["container", "inspect", name]
  return $ case decode podInspect of
    Just [container] -> Just container
    _ -> Nothing

isContainer :: String -> IO Bool
isContainer name = isJust <$> cmdMaybe "podman" ["container", "exists", name]

-- | Convenient bind unless wrapper for the `if "test in IO" then "do this IO" pattern`
--
-- unless        :: Applicative f => Bool -> f () -> f ()
-- flip          ::    (a -> b -> c) -> b -> a    -> c
-- (flip unless) :: Applicative f => f () -> Bool -> f ()
--
-- For example to print something if a file does not exist:
-- (flip unless (print "Nop")) :: Bool     -> IO ()
-- (doesFileExist)             :: FilePath -> IO Bool
-- (doesFileExist "/test" >>=) :: (Bool -> IO b) -> IO b
-- >>> doesFileExist "/test" `bunless` print "Nop!"
-- [prints Nop!]
bunless :: IO Bool -> IO () -> IO ()
a `bunless` b = a >>= flip unless b
-- fixity value ensure this get evaluated last
infixl 0 `bunless`

-- | Install zuul in a venv using pip
getPipList :: IO [String]
getPipList =
  do
    doesDirectoryExist "venv" `bunless` cmd_ "python3" ["-m", "venv", "venv"]
    doesFileExist "/usr/include/re2/re2.h" `bunless` cmd_ "sudo" ["dnf", "install", "-y", "re2-devel"]
    mapM_ ensure ["zuul", "nodepool", "ansible"]
    lines <$> cmd "./venv/bin/pip3" ["freeze"]
  where
    ensure name = doesFileExist ("venv/bin/" <> name) `bunless` cmd_ "./venv/bin/pip3" ["install", name]

-- | Install zuul in a container using software factory rpm
getRpmList :: IO [String]
getRpmList =
  do
    isContainer "sf" `bunless` cmd_ "podman" ["create", "--name", "sf", "registry.centos.org/centos:7", "sleep", "Inf"]
    containerRunning . containerState . fromJust <$> inspectContainer "sf" `bunless` cmd_ "podman" ["start", "sf"]
    (== "master") . fromMaybe "" <$> runMaybe ["cat", "/etc/sf-release"] `bunless` run_ ["yum", "install", "-y", masterRpm]
    ensure "zuul" ["zuul-merger", "zuul-web", "zuul-executor", "zuul-scheduler"]
    ensure "nodepool" ["nodepool-launcher", "nodepool-builder"]
    lines <$> run ["rpm", "-qa"]
  where
    ensure name req = doesExistInContainer name `bunless` run_ (["yum", "install", "-y"] <> req)
    doesExistInContainer name = isJust <$> runMaybe ["test", "-f", "/usr/bin/" <> name]
    runContainer c arg = c "podman" (["exec", "sf"] <> arg)
    runMaybe = runContainer cmdMaybe
    run_ = runContainer cmd_
    run = runContainer cmd

masterRpm :: String
masterRpm = "https://softwarefactory-project.io/kojifiles/repos/sf-master-el7/Mash/sf-release-9999.14.g1439b64-14.el7.noarch.rpm"

main :: IO ()
main = do
  doesFileExist "pip.txt" `bunless` unlines <$> getPipList >>= writeFile "pip.txt"
  doesFileExist "rpm.txt" `bunless` unlines <$> getRpmList >>= writeFile "rpm.txt"
  -- TODO: translate pip to rpm and use https://hackage.haskell.org/package/pkgtreediff
