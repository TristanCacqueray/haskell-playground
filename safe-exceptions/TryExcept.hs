-- |
-- How to try: except: all the exceptions
module TryExcept where

import Control.Exception.Safe
import Control.Monad

data AppException = InvalidJSONException deriving (Show)

instance Exception AppException

unsafeFunction :: Int -> IO Int
unsafeFunction 42 = error "h2g2"
unsafeFunction 41 = throw InvalidJSONException
unsafeFunction x = pure $ x + 1

worker :: IO ()
worker = mapM_ (print <=< unsafeFunction) [30 .. 50]

-- | Easy but does not distinguished the exceptions
safeWorkerTry :: IO ()
safeWorkerTry = do
  res <- tryAny worker
  case res of
    Left e -> print $ "Got: " <> show e
    Right x -> pure x

-- | More verbose but handle custom exceptions
safeWorkerCatch :: IO ()
safeWorkerCatch = worker `catches` [Handler handleOurException, Handler handleOtherException]
  where
    handleOurException :: AppException -> IO ()
    handleOurException e = putStrLn $ "App throw: " <> show e
    handleOtherException :: SomeException -> IO ()
    handleOtherException e = putStrLn $ "App crashed: " <> show e
