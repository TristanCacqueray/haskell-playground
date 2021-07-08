-- |
module SimpleExcept where

import Control.Monad (forever)
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader

data Env = Env
  { host :: String,
    port :: Int
  }

type Shell a = ReaderT Env (ExceptT String IO) a

putStr' :: MonadIO m => String -> m ()
putStr' = liftIO . putStr

checkResult :: String -> Shell Bool
checkResult result = case result of
  "ok" -> pure True
  "ko" -> pure True
  "fail" -> throwError "Oops it failed"
  _ -> lift $ lift (read <$> getLine)

prompt :: Shell String
prompt = do
  host' <- asks host
  putStr' $ host' <> "> "
  x <- liftIO getLine
  liftIO $ putStrLn "ok now running..."
  when (x == "exit") (throwError "this is the end")
  pure x

shell :: Shell ()
shell = do
  lift $ lift $ putStrLn "Starting"
  forever prompt
  lift $ lift $ putStrLn "Bie :0)"

main :: IO ()
main = runExceptT $ runReaderT shell (Env "localhost" 4242)
