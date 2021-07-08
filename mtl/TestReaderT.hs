-- |
module TestReaderT where

import Control.Monad.Except
import Control.Monad.Reader

newtype Age = MkAge Int

newtype Name = MkName String

-- type ReaderT r m a
--              | |  \ the final value
--              | |- the inner monad
--              |- the value available with ask
type TestM a = ReaderT Age (ReaderT Name (ExceptT String (ExceptT Int IO))) a

printInfo :: TestM ()
printInfo = do
  MkAge age <- ask
  MkName name <- lift ask
  lift . lift . lift $ throwError 42
  throwError "oops"
  liftIO $ print $ "Age: " <> show age <> ", name: " <> name
