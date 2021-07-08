-- |
module Break where

import Streaming (Of (..), Stream (..), chunksOf)
import qualified Streaming.Prelude as S

data Change = Change
  { date :: Int,
    message :: String
  }
  deriving (Eq, Show)

fetch :: Monad m => Stream (Of Change) m ()
fetch = go 1
  where
    go pos = do
      S.yield (Change pos $ "Message: " <> show pos)
      go (pos + 1)

getUntil :: Monad m => Int -> Stream (Of Change) m () -> Stream (Of Change) m ()
getUntil limit stream = const () <$> S.break limitReached stream
  where
    limitReached :: Change -> Bool
    limitReached (Change date message)
      | date > limit = True
      | otherwise = False

streamFetch :: Monad m => Stream (Of Change) m ()
streamFetch = getUntil 5 $ go
  where
    go = do
      fetch
      go

main :: IO ()
main = S.print streamFetch
