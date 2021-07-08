{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
module FoldBreak where

import Control.Monad
import Control.Monad.Except (MonadError, throwError)
import Relude
import Streaming (Of (..), Stream (..), chunksOf)
import qualified Streaming.Prelude as S

data Change = Change
  { date :: Int,
    message :: String
  }
  deriving (Eq, Show)

newtype LentilleM a = LentilleM {unLentille :: ExceptT String IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)
  deriving newtype (MonadError String)

fetch :: Stream (Of Change) LentilleM ()
fetch = go 1
  where
    go 5 = throwError "Oops"
    go pos = do
      S.yield (Change pos $ "Message: " <> show pos)
      go (pos + 1)

main :: IO ()
main = do
  res <- runExceptT $ unLentille $ S.print $ S.take 10 fetch
  case res of
    Left res -> putStrLn $ "It failed: " <> show res
    Right x -> putStrLn "Todo bueno"
