{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module EffectfulPlayground where

import Data.Map (Map)
import Data.Map qualified as M
import Data.Time.Calendar.WeekDate
import Data.Time.Clock
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Static
import Effectful.State.Static.Local
import GHC.Generics

demo :: IO ()
demo = do
  putStrLn "Test application"
  print (runApp appFunc)
  putStrLn "\nReal application"
  print =<< runAppIO appFunc

-------------------------------------------------------------------------------
-- A custom effect for http request
-------------------------------------------------------------------------------
type Request = String

type Response = String

data HttpEffect :: Effect where
  GetHTTP :: Request -> HttpEffect m Response

type instance DispatchOf HttpEffect = 'Dynamic

runHttpEffectIO :: IOE :> es => Eff (HttpEffect : es) a -> Eff es a
runHttpEffectIO = interpret $ \_ -> \case
  GetHTTP req -> liftIO do
    putStrLn $ "Curling " <> req
    pure "response"

getHTTP :: HttpEffect :> es => Request -> Eff es Response
getHTTP = send . GetHTTP

testHttpEffect :: HttpEffect :> es => Eff es Response
testHttpEffect = do
  getHTTP "localhost"

-------------------------------------------------------------------------------
-- Application runner
-------------------------------------------------------------------------------
data AppState = AppState
  { packages :: [String],
    packagesCabalStore :: Map String Int
  }

data AppEnv = AppEnv
  { packagesUrl :: String,
    refreshPackages :: Bool
  }

runAppIO :: Eff '[State AppState, Reader Int, Reader AppEnv, HttpEffect, IOE] a -> IO a
runAppIO = runEff . runHttpEffectIO . runReader @AppEnv env . runReader @Int 1 . evalState st
  where
    env = AppEnv "localhost" True
    st = AppState [] mempty

-------------------------------------------------------------------------------
-- Pure application runner
-------------------------------------------------------------------------------
runPureHttpEffect :: Map Request Response -> Eff (HttpEffect : es) a -> Eff es a
runPureHttpEffect mock = reinterpret (evalState mock) $ \_ -> \case
  GetHTTP req ->
    gets (M.lookup req) >>= \case
      Just contents -> pure contents
      Nothing -> error $ "Bad mock" <> req

runApp :: Eff '[State AppState, Reader Int, Reader AppEnv, HttpEffect] a -> a
runApp = runPureEff . runPureHttpEffect mock . runReader @AppEnv env . runReader @Int 1 . evalState st
  where
    mock = (M.fromList [("localhost/v1/zuul", "mockResp")])
    env = AppEnv "localhost" True
    st = AppState [] mempty

-------------------------------------------------------------------------------
-- Application code
-------------------------------------------------------------------------------
appFunc :: (HttpEffect :> es, Reader Int :> es, Reader AppEnv :> es, State AppState :> es) => Eff es (String, String, String)
appFunc = do
  zuulUrl <- addPackageAndGetURL "zuul"
  nodepoolUrl <- addPackageAndGetURL "nodepool"
  zuulVersion <- getHTTP zuulUrl
  pure (zuulUrl, nodepoolUrl, zuulVersion)

addPackageAndGetURL :: (Reader Int :> es, Reader AppEnv :> es, State AppState :> es) => String -> Eff es String
addPackageAndGetURL pkg = do
  addPackage pkg
  baseUrl <- asks packagesUrl
  version <- ask @Int
  pure $ baseUrl <> "/" <> "v" <> show version <> "/" <> pkg

addPackage :: State AppState :> es => String -> Eff es ()
addPackage pkg = modify (\s -> s {packages = pkg : packages s})
