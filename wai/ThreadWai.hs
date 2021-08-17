{-# LANGUAGE OverloadedStrings #-}

-- |
-- A wai app to test multi thread capabilities.
-- Try with `-threaded -rtsopts -with-rtsopts=-N`
module Main where

import Control.Concurrent
import Data.Text (pack)
import GHC.IO.Handle (hFlush)
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Say
import System.IO (stdout)

testApp :: Wai.Request -> (Wai.Response -> IO a) -> IO a
testApp req resp = do
  th <- myThreadId
  say $ "Serving request from: " <> pack (show th)
  hFlush stdout
  resp $ Wai.responseLBS HTTP.ok200 [] mempty

main :: IO ()
main = do
  num <- getNumCapabilities
  say $ "Serving on 3000, with cap: " <> pack (show num)
  hFlush stdout
  Warp.run 3000 testApp
