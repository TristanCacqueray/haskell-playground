-- |
module BreakEither where

import Control.Monad.Trans (lift)
import Streaming (Of (..), Stream (..), chunksOf)
import qualified Streaming.Prelude as S

data Change = Change
  { date :: Int,
    message :: String
  }
  deriving (Eq, Show)

fetch :: Monad m => Stream (Of (Either String Change)) m ()
fetch = go 1
  where
    go 5 = S.yield (Left "This is the end") >> go 6
    go pos = do
      S.yield (Right $ Change pos $ "Message: " <> show pos)
      go (pos + 1)

postError :: Stream (Of String) (Stream (Of Change) IO) () -> Stream (Of Change) IO ()
postError = S.effects . S.mapM (\i -> lift $ putStrLn $ "HERE: " <> i)

main :: IO ()
main = do
  let rest = postError $ S.partitionEithers $ S.take 10 fetch
  S.print (S.mapped S.toList $ chunksOf 3 rest)
