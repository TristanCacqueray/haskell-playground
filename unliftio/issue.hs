-- |
module Issue where

import Control.Monad.Reader
import System.IO

-- withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r

testAction :: Handle -> ReaderT Int IO ()
testAction = undefined

testApp :: ReaderT Int IO ()
testApp =
  -- This does not work: Expected IO (), got ReaderT ...
  -- liftIO $ withFile "/tmp/test" ReadMode testAction
  ReaderT $ \env -> withFile "/tmp/test" ReadMode (\handle -> runReaderT (testAction handle) env)

-- | Note: ReaderT can be created manually like so:
manualReader :: ReaderT Int IO ()
manualReader = ReaderT $ \count -> print count

-- this is equivalent to:
-- _ = do
--  count <- ask
--  liftIO $ print count
