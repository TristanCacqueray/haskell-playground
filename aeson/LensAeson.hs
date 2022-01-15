#! /usr/bin/env nix-shell
#! nix-shell --pure -p "haskellPackages.ghcWithPackages (p: with p; [ with-utf8 lens-aeson relude ])" -i runghc

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | The goal is to display the keys of an objects list.
-- Using lens-aeson, this demo manipulate json value to show:
--
-- # ./LensAeson.hs
-- 42: ["name","timestamp"]
-- 0: ["other"]
module LensAeson where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Relude

examples =
  [ "{\"timestamp\": 42, \"name\": \"demo\"}",
    "{\"other\": null}"
  ]

-- | Get object attributes
getAttributes :: Value -> [Text]
getAttributes v = (toListOf $ members . asIndex) v

showValue :: Value -> Text
showValue v = show ts <> ": " <> show (getAttributes v)
  where
    ts = fromMaybe 0 $ v ^? key "timestamp" . _Integer

main :: IO ()
main = do
  let docs = case traverse eitherDecode examples of
        Right x -> x
        Left x -> error (show x)
  traverse_ putTextLn $ map showValue docs
