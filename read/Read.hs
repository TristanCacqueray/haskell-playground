-- | The goal is to read relative date value such as "now-3weeks" or "now"
module Read where

import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec (lift)
import Text.Read (readPrec)
import Text.Read.Lex (readDecP)

data TimeRange = Hour | Day | Week
  deriving (Show)

timeRangeReader :: ReadP TimeRange
timeRangeReader = (hourR <|> dayR <|> weekR) <* optional (char 's')
  where
    hourR = Hour <$ string "hour"
    dayR = Day <$ string "day"
    weekR = Week <$ string "week"

instance Read TimeRange where
  readPrec = lift timeRangeReader

data RelativeTime = MkRelativeTime Word TimeRange
  deriving (Show)

relTimeReader :: ReadP RelativeTime
relTimeReader = string "now" *> (relR <|> pure defTime)
  where
    defTime = MkRelativeTime 0 Hour
    relR = char '-' *> (MkRelativeTime <$> countR <*> timeRangeReader)
    countR :: ReadP Word
    countR = readDecP

instance Read RelativeTime where
  readPrec = lift relTimeReader

-- >>> read "now" :: RelativeTime
-- MkRelativeTime 0 Hour
