{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
module TestReaderT where

import Control.Concurrent (threadDelay)
import Control.Monad.Except
import Control.Monad.Reader

newtype Age = MkAge Int

newtype Name = MkName String

data Env = Env
  { envName :: Name,
    envLog :: String -> IO ()
  }

class MonadLogger m where
  doLog :: String -> m ()

instance MonadLogger (ReaderT Env IO) where
  doLog txt = do
    logger <- asks envLog
    liftIO $ logger txt

class MonadPrint m where
  doPrint :: String -> m ()

instance MonadPrint (ReaderT Env IO) where
  doPrint = liftIO . putStrLn

getName :: MonadPrint m => MonadReader Env m => m String
getName = do
  MkName name <- asks envName
  doPrint "getting name"
  pure name

testReader :: ReaderT Env IO ()
testReader = do
  name <- getName
  liftIO $ print name
  liftIO $ threadDelay 1000000

-- type ReaderT r m a
--              | |  \ the final value
--              | |- the inner monad
--              |- the value available with ask
type TestM a = ReaderT Age (ReaderT Name (ExceptT String (ExceptT Int IO))) a

printInfo :: TestM ()
printInfo = do
  MkAge age <- ask
  MkName name <- lift ask
  lift . lift . lift $ throwError 42
  throwError "oops"
  liftIO $ print $ "Age: " <> show age <> ", name: " <> name
