-- | This playground shows the rio pattern, from the ground up

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Control.Monad.IO.Class
import Control.Monad.Reader
import Lens.Micro
import Lens.Micro.Mtl
import qualified Streaming.Prelude as S
import Prelude

-- | The main context
newtype Monocle env a = Monocle (ReaderT env IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader env)

runMonocle :: env -> Monocle env a -> IO a
runMonocle env (Monocle (ReaderT f)) = f env

-- | With two environments, e.g. one for a API, and one for a Worker
data App = App
  { appLogFunc :: Logger,
    appName :: String
  }
data Worker = Worker
  { workerLogFunc :: Logger,
    workerName :: String,
    _workerClient :: AppClient
  }

-- | And some fake types
data AppClient = AppClient
type Logger = String -> IO ()


-- | The desired helper:
-- PRO: best error message
-- CON: can't be used by the Worker env
logMessageConcret :: String -> Monocle App ()
logMessageConcret msg = do
  appName <- view (to appName)
  logInfo $ "Hello from:" <> appName
  logInfo msg


-- | Another implementation, but using Has* constraint
-- PRO: works with both env
-- CON: needs Has* instance
logMessage :: (HasLog env, HasAppName env) => String -> Monocle env ()
logMessage msg = do
  appName <- view appNameL
  logInfo $ "Hello from:" <> appName
  logInfo msg
  liftIO $ pure ()

-- | Another implementation using MonadReader
-- PRO: works without Monocle
-- CON: needs complex MultiParamTypeClasses
logMessageNoMonocle :: (MonadIO m, MonadReader env m, HasLog env, HasAppName env) => String -> m ()
logMessageNoMonocle msg = do
  appName <- view appNameL
  logInfo $ "Hello from:" <> appName
  logInfo msg

-- | Last implementation, using a special typeclass to remove the MonadIO
-- PRO: most generic
logMessageNoMonocleNoIO :: MonadReader env m => MyLog m => HasAppName env => String -> m ()
logMessageNoMonocleNoIO msg = do
  appName <- view appNameL
  myLog $ "Hello from:" <> appName
  myLog msg


-- | HasLog defines a getter for a logger function (which runs in IO), to be used with logInfo
class HasLog a where
  appLogL :: Lens' a Logger

instance HasLog App where
  appLogL = lens appLogFunc (\app lf -> app {appLogFunc = lf})

instance HasLog Worker where
  appLogL = lens workerLogFunc (\app lf -> app {workerLogFunc = lf})

-- | HasAppName defines a getter for the application name
class HasAppName a where
  appNameL :: Lens' a String

instance HasAppName App where
  appNameL = lens appName (\app n -> app {appName = n})

instance HasAppName Worker where
  appNameL = lens workerName (\app n -> app {workerName = n})


-- | MyLog defines a logger function
class MyLog m where
  myLog :: String -> m ()

instance MyLog (Monocle App) where
  myLog msg = do
    logger <- view appLogL
    liftIO $ logger msg

instance MyLog (Monocle Worker) where
  myLog msg = do
    logger <- view appLogL
    liftIO $ logger msg


-- | The same strategy can be used for Streaming
streamerConcret :: S.Stream (S.Of Bool) (Monocle Worker) ()
streamerConcret = do
  logInfo "Starting stream"
  S.each [True, False, True]
  logInfo "Closing stream"
  S.each [False, False]

streamer :: HasLog env => HasAppName env => S.Stream (S.Of Bool) (Monocle env) ()
streamer = do
  logInfo "Starting stream"
  S.each [True, False, True]
  logInfo "Closing stream"
  S.each [False, False]


streamerNoMonocle :: MonadIO m => MonadReader env m => HasLog env => HasAppName env => S.Stream (S.Of Bool) m ()
streamerNoMonocle = do
  logInfo "Starting stream"
  S.each [True, False, True]
  logInfo "Closing stream"
  S.each [False, False]


-- note: here we need to use `lift $ myLog` because Stream does not automatically implement MyLog
streamerNoMonocleNoIO :: MonadReader env m => MyLog m => HasAppName env => S.Stream (S.Of Bool) m ()
streamerNoMonocleNoIO = do
  lift $ myLog "Starting stream"
  S.each [True, False, True]
  lift $ myLog "Closing stream"
  S.each [False, False]


main :: IO ()
main = do
  runMonocle (App putStrLn "api") $ do
    logMessage "api start"
    logMessageNoMonocle "api start"
    logMessageNoMonocleNoIO "api no io"
  runMonocle (Worker putStrLn "worker" AppClient) $ do
    logMessage "worker start"
    logMessageNoMonocleNoIO "worker no io"
    res <- S.toList streamerConcret
    liftIO $ print res

myLogInfo :: MyLog m => String -> m ()
myLogInfo msg = myLog msg

logInfo :: (MonadIO m, MonadReader env m, HasLog env) => String -> m ()
logInfo msg = do
  logger <- view appLogL
  liftIO $ logger msg
