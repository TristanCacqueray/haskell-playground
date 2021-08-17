{-# LANGUAGE FlexibleContexts #-}

-- |
module HaskellKind where

import Control.Monad.Reader
import Data.Maybe
import System.Environment

validKey :: [String]
validKey = ["KEY1", "KEY2"]

isValideKey :: String -> ReaderT [String] IO Bool
isValideKey key = do
  keys <- ask
  keyExist <- lift (isJust <$> lookupEnv key)
  pure $ keyExist && key `elem` keys

isValideKey' :: MonadIO m => MonadReader [String] m => String -> m Bool
isValideKey' key = do
  keys <- ask
  keyExist <- liftIO (isJust <$> lookupEnv key)
  pure $ keyExist && key `elem` keys

validate :: Maybe String -> IO Bool
validate (Just n) = lookupEnv n >>= pure . isJust
validate Nothing = pure False
