module TestExceptT where

import Control.Monad.Except (ExceptT, lift, throwError)

getGroupIO :: IO [String]
getGroupIO = do
  putStrLn "Getting group"
  error "Oops, something went wrong"
  pure ["core", "ptl"]

getGroup :: ExceptT String IO [String]
getGroup = do
  lift $ putStrLn "Getting group"
  throwError "Oops, something went wrong"
  pure ["core", "ptl"]

type IOE = ExceptT String IO

getGroup' :: IOE [String]
getGroup' = getGroup

main :: IO ()
main = runExceptT getGroup
