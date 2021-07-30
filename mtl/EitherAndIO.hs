-- |
module EitherAndIO where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, except, runExceptT)
import System.Environment (getEnv)

data User = User {name :: String, email :: String} deriving (Show)

orFail :: Maybe a -> String -> Either String a
orFail Nothing e = Left e
orFail (Just a) _ = Right a

orFailT a = except . orFail a

validate :: Maybe String -> Maybe String -> Either String User
validate nameM emailM = do
  name <- nameM `orFail` "no name"
  email <- emailM `orFail` "no mail"
  pure (User name email)

validateWithEnv :: Maybe String -> Maybe String -> ExceptT String IO User
validateWithEnv nameM emailM = do
  nameEnv <- nameM `orFailT` "no name"
  name <- lift $ getEnv nameEnv
  email <- emailM `orFailT` "no mail"
  pure (User name email)
