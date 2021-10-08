{-# LANGUAGE OverloadedStrings #-}

-- | Some example usage of Attoparsec to parse gerrit event's message
module Commit where

import Control.Applicative (optional, (<|>))
import Control.Monad (void)
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.Text
import Data.Char (isDigit, isSpace)
import Data.Text (Text)
import qualified Data.Text

testMessage :: Text
testMessage =
  mconcat
    [ "Patch Set 3: Verified+2 Workflow-2 -Code-Review",
      "\n\n",
      "Build succeeded"
    ]

-- | The simplest version which does not decode the approvals
-- >>> parseOnly simpleMessageParser testMessage
-- Right ["Verified+2","Workflow-2","-Code-Review"]
simpleMessageParser :: Parser [Text]
simpleMessageParser = do
  void $ string "Patch Set "
  void $ decimal
  void $ string ": "
  word `sepBy1` (char ' ')
  where
    word = takeWhile1 (not . isSpace)

-- | The complete implementation:
-- >>> parseOnly messageParser testMessage
-- Right (3,Just [("Verified",2),("Workflow",-2),("Code-Review",0)])
messageParser :: Parser (Int, Maybe [(Text, Int)])
messageParser = do
  _ <- string "Patch Set "
  nr <- decimal
  _ <- char ':'
  approval <- optional (char ' ' *> approvalParser `sepBy1` (char ' '))
  _ <- char '\n'
  pure (nr, approval)

-- | Parse an approval atom:
-- >>> parseOnly approvalParser "Code-Review+2"
-- Right ("Code-Review",2)
-- >>> parseOnly approvalParser "-Workflow"
-- Right ("Workflow",0)
approvalParser :: Parser (Text, Int)
approvalParser = do
  removed <- option Nothing (Just <$> char '-')
  (name, sign) <- approvalName
  value <- case removed of
    Just _ -> pure 0
    Nothing -> decimal
  pure (name, sign * value)

-- | Parse an approval name and the sign of its value
-- >>> parseOnly approvalName "Workflow+"
-- Right ("Workflow",1)
-- >>> parseOnly approvalName "Workflow-"
-- Right ("Workflow",-1)
-- >>> parseOnly approvalName "Workflow"
-- Right ("Workflow",1)
approvalName :: Parser (Text, Int)
approvalName = do
  name <- takeWhile1 approvalChar
  pure $ case Data.Text.last name of
    '-' -> (Data.Text.init name, -1)
    '+' -> (Data.Text.init name, 1)
    _ -> (name, 1)
  where
    approvalChar c = not (isDigit c) && c `notElem` [' ', '\t', '\n']

-- | Another implementation using lookAhead for the approval name
messageParser' :: Parser (Int, Maybe [(Text, Int)])
messageParser' = do
  _ <- string "Patch Set "
  nr <- decimal
  _ <- char ':'
  approval <- optional (char ' ' *> approvalParser' `sepBy1` (char ' '))
  _ <- char '\n'
  pure (nr, approval)

approvalParser' :: Parser (Text, Int)
approvalParser' = do
  removed <- optional (char '-')
  -- the benefit of lookAhead is that we have a better semantic here
  name <- approvalName'
  value <- case removed of
    Just _ -> pure 0
    Nothing -> signed decimal
  pure (name, value)

-- | Parse an approval name, stopping before the value
-- >>> parseOnly approvalName' "Code-Review+2"
-- Right "Code-Review"
approvalName' :: Parser Text
approvalName' = do
  char <- satisfy (not . isSpace)
  -- check if next chars are a signed number
  nextIsNotApproval <- lookAhead (optional notApprovalName)
  case nextIsNotApproval of
    Just _ -> pure $ Data.Text.singleton char -- stop here
    Nothing -> Data.Text.cons char <$> approvalName' -- continue recursively
  where
    notApprovalName = () <$ signed decimal <|> () <$ space <|> endOfLine <|> endOfInput
