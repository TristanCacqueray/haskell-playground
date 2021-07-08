-- |
module StateMonad where

import Control.Monad.State

type CounterState = State Int

addId :: String -> CounterState String
addId name =
  get >>= \idx -> case name of
    "'children" -> put (idx + 1) >> return (name ++ "-" ++ show idx)
    _ -> return name

statefulCalculation :: State Int [String]
statefulCalculation = mapM addId ["'children", "'children", "unit", "tata"]

main :: IO ()
main = putStrLn (show (runState statefulCalculation 0))
