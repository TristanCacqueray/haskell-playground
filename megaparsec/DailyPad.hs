#! /usr/bin/env nix-shell
#! nix-shell --pure -p "haskellPackages.ghcWithPackages (p: with p; [ relude megaparsec pretty-simple with-utf8 optparse-generic ])" -i runghc

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | The goal is to parse a daily pad that looks like this:
-- ```
-- 2016-04-20
-- #info alice task
-- #info alice multi
-- line task
--
-- #info bob other task
--
-- 2016-04-19
--
-- #info a b
-- ```
--
-- And create an org agenda like that:
-- ```
-- * 2016
-- ** 2016-04 April
-- *** 2016-04-20 Sunday
-- **** task
-- **** multi
-- line task
-- ```
module DailyList where

import Data.Char (isAlpha)
import Data.List (nub)
import Data.Text qualified
import Data.Time.Calendar
import Data.Time.Format (defaultTimeLocale, formatTime)
import Main.Utf8 (withUtf8)
import Options.Generic
import Relude hiding (many, some)
import System.Environment (getArgs)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (space)
import Text.Megaparsec.Char.Lexer hiding (space)
import Text.Pretty.Simple (pPrint)

-- $setup
-- >>> let runParser' p = putStrLn . either errorBundlePretty show . runParser p ""

-- | Parse a date like YYYY-MM-DD or DD-MM-YYYY
-- >>> runParser' dateParser "1980-01-01"
dateParser :: Parser Day
dateParser = do
  n1 <- decimal
  "-"
  n2 <- decimal
  "-"
  n3 <- decimal
  optional ":"
  if
      | n1 > 1900 && n2 < 13 && n3 < 32 -> pure $ fromGregorian (toInteger n1) n2 n3
      | n1 < 32 && n2 < 13 && n3 > 1900 -> pure $ fromGregorian (toInteger n3) n2 n1
      | otherwise -> fail "invalid date"

-- | Event is a data type
data Event = Event
  { author :: Text,
    action :: Text
  }
  deriving (Show, Ord, Eq)

-- | Parse a event
-- >>> runParser' eventParser "#info tristanC hello"
-- >>> runParser' eventParser "** DONE test"
eventParser :: Parser Event
eventParser = do
  author <- commonEvent <|> pepperEvent
  event <- multiLineEvent <?> "Event"
  pure $ Event author event
  where
    -- Parse the `#action author` bit
    commonEvent = do
      "# " <|> "#"
      takeWhile1P Nothing isAlpha <?> "action"
      some " "
      author <- takeWhile1P Nothing isAlpha <?> "author"
      " " <$ some " " <|> ": "
      pure author

    -- some write entry using `** action` syntax, without authoring. Let's call them pepper.
    pepperEvent = do
      "** " <|> "*** "
      pure "pepper"

    multiLineEvent =
      "" <$ lookAhead done <|> do
        event <- takeWhile1P Nothing (/= '\n')
        "\n"
        next <- multiLineEvent
        pure $ event <> "\n" <> next

    -- multi lines event stops when the next line looks like this
    done = "\n" <|> "#" <|> ("" <$ try dateParser) <|> ("" <$ eof)

-- | JournalDay is a data type for full day
data JournalDay = JournalDay
  { date :: Day,
    events :: [Event]
  }
  deriving (Show, Eq, Ord)

-- | Parse a complete day
dayParser :: Parser JournalDay
dayParser = do
  optional (many header) <?> "Header"
  date <- lex dateParser <?> "Date"
  events <- many (lex eventParser) <?> "Events"
  pure $ JournalDay date events
  where
    header = dashSep <|> sprintSep <|> magicSep <|> "\n"
    dashSep = ("--" <|> "==") >> rest
    sprintSep = do
      "Sprint" <|> "sprint"
      rest
    magicSep = do
      "***" >> rest
      "* Sprint" >> rest
      "***" >> rest

-- | Parse a complete daily file
dailyFileParser :: Parser [JournalDay]
dailyFileParser = do
  xs <- many (lex dayParser)
  eof
  pure xs

-- | Format agenda headers when needed
formatHeaders :: Maybe Day -> Day -> [Text]
formatHeaders prevDateM curDate =
  let (curYear, curMonth, _) = toGregorian curDate
      yearTitle = "* " <> show curYear
      monthTitle = toText $ "** " <> formatTime defaultTimeLocale "%Y-%m %B" curDate
   in case prevDateM of
        Nothing -> ["", yearTitle, monthTitle]
        Just prevDate ->
          let (prevYear, prevMonth, _) = toGregorian prevDate
           in [""]
                <> bool [] [yearTitle] (curYear /= prevYear)
                <> bool [] [monthTitle] (curMonth /= prevMonth)

-- | Format a single day
formatDay :: JournalDay -> State (Maybe Day) [Text]
formatDay JournalDay {..} = do
  prevDateM <- get
  let headers = formatHeaders prevDateM date
      dateTitle = toText $ showGregorian date <> " " <> show (dayOfWeek date)
  put (Just date)
  pure $ headers <> ["", "*** " <> dateTitle, ""] <> concatMap formatEvent events
  where
    formatEvent :: Event -> [Text]
    formatEvent Event {..} = ["**** " <> title] <> body
      where
        (firstLine, body) = case Data.Text.lines action of
          (x : xs) -> (x, xs)
        title = Data.Text.dropWhile (`elem` [' ', '-']) firstLine

-- | Format the complete agenda for a given author
formatAgenda :: Text -> [JournalDay] -> [Text]
formatAgenda author days = concat $ evalState (traverse formatDay days') Nothing
  where
    days' = reverse (keepAuthor author days [])

-- | Filter days where author is present
keepAuthor :: Text -> [JournalDay] -> [JournalDay] -> [JournalDay]
keepAuthor user days acc = case days of
  [] -> acc
  (curDay : xs) ->
    let authorEvents = filter (\ev -> author ev == user) (events curDay)
        newDay = case authorEvents of
          [] -> []
          _ -> [JournalDay (date curDay) authorEvents]
     in keepAuthor user xs (acc <> newDay)

formatStats :: [JournalDay] -> [Text]
formatStats days =
  [ "Dates:   " <> show (length days),
    "Entries: " <> show (length entries),
    "Authors: " <> show (length authors)
  ]
    <> map (mappend "- ") (sort authors)
  where
    entries = concatMap events days
    authors = nub $ map author entries

data CLI = CLI {file :: FilePath, stat :: Bool, extract :: Maybe Text}
  deriving (Generic, Show)

instance ParseRecord CLI

main :: IO ()
main = withUtf8 $ do
  args <- getRecord "Parse daily"
  days <- getEntries (file args)
  if stat args
    then printStat days
    else case extract args of
      Just author | not (stat args) -> printAgenda author days
      _ -> putTextLn "--extract or --stat needs to be set"
  where
    printStat = traverse_ putTextLn . formatStats
    printAgenda author = traverse_ putTextLn . formatAgenda author
    getEntries fp = do
      content <- readFile fp
      case runParser dailyFileParser fp (toText content) of
        Right entries -> pure entries
        Left e -> do
          putStrLn $ errorBundlePretty e
          exitFailure

-- | Helper function to consume space before and after
lex :: Parser a -> Parser a
lex = lexeme space

-- | Helper to consume the rest of a line
rest :: Parser Text
rest = do
  takeWhileP Nothing (/= '\n')
  "\n"

-- | Type synonims to simplify function signature
type Parser a = ParsecT PError Text Identity a

-- | A newtype for error to enable custom message
newtype PError = PError Text deriving (Show, Ord, Eq)

instance ShowErrorComponent PError where
  showErrorComponent = show
