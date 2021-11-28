#!/usr/bin/env stack
{- stack
    --resolver lts-16.3
    script
    --package req
    --package http-client
    --package http-client-tls
    --package connection
    --package text
    --package optparse-generic
    --package modern-uri
 -}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- hcurl demonstrate Network.HTTP.Req like a curl cli

import Control.Monad.IO.Class
import Data.Maybe
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Network.Connection (TLSSettings (TLSSettingsSimple))
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.HTTP.Req
import Options.Generic
import Text.URI (mkURI)

data Curl = Curl {insecure :: Bool, url :: Text}
  deriving (Generic, Show)

instance ParseRecord Curl

-- | This manager skip tls verification
insecureTLSConfig :: IO HttpConfig
insecureTLSConfig = do
  myManager <- newManager $ mkManagerSettings (TLSSettingsSimple True False False) Nothing
  return $ defaultHttpConfig {httpConfigAltManager = Just myManager}

main :: IO ()
main =
  do
    args <- getRecord "HCurl - Network.HTTP.Req playground"
    uri <- mkURI $ url args
    let schemeAndOption = fromMaybe (error $ "Invalid url: " <> show (url args)) (useURI uri)
    config <-
      if insecure args
        then insecureTLSConfig
        else return defaultHttpConfig
    runReq config (doReq GET schemeAndOption NoReqBody bsResponse)
  where
    doReq method schemeAndOption body response = do
      -- http or https is defined at compile time: see https://github.com/mrkkrp/req/issues/78
      r <- case schemeAndOption of
        -- http case
        Left (url', option) -> req method url' body response option
        -- https case
        Right (url', option) -> req method url' body response option

      liftIO $ putStrLn $ unpack $ decodeUtf8With lenientDecode $ responseBody r
