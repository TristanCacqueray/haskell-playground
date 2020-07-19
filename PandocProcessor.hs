#!/usr/bin/env stack
{- stack
    --resolver lts-16.3
    script
    --package pandoc
    --package pandoc-types
    --package text
 -}

{-# LANGUAGE OverloadedStrings #-}

-- | Process pandoc neutral format
-- https://hackage.haskell.org/package/pandoc-types-1.17.5
-- https://hackage.haskell.org/package/pandoc-2.5
module PandocProcessor where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)
import Text.Pandoc
import Text.Pandoc.Definition

consecutiveCodeBlockSideBySide :: PandocMonad m => Text -> m Text
consecutiveCodeBlockSideBySide src =
  do
    (Pandoc meta raw) <- readMarkdown readerOptions src
    let converted = foldr go [] raw
    writeRST writerOptions (Pandoc meta converted)
  where
    readerOptions = def {readerExtensions = githubMarkdownExtensions}
    writerOptions = def {writerColumns = 200, writerWrapText = WrapPreserve}
    go m@(CodeBlock _ _) acc =
      case acc of
        (n@(CodeBlock _ _) : xs) -> mkTable m n : xs
        xs -> m : xs
      where
        mkTable m n = Table [] align [0.5, 0.5] [] [[[m], [n]]]
        align = [AlignLeft, AlignLeft]
    go e acc = e : acc

main = do
  args <- getArgs
  case args of
    ["--side-by-side-code-table", src, header, dst] -> do
      src' <- T.readFile src
      dst' <- runIOorExplode (consecutiveCodeBlockSideBySide src')
      header' <- T.readFile header
      T.writeFile dst (header' <> "\n" <> dst')
    _ -> print "usage: src header dst"
