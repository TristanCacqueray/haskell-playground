-- | The goal is to traverse until a condition is met
module TraverseMaybeT where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Bool (bool)

type Workspace = String

-- | Context: the goal is to run this function until it returns True
validUser :: Workspace -> IO Bool
validUser ws = case ws of
  "good" -> do
    putStrLn $ "Found: " <> ws
    pure True
  _ -> do
    putStrLn $ "Not found: " <> ws
    pure False

-- | Using traverse is not efficient because we want to stop early:
-- >>> traverse validUser ["unknown", "good", "bad"]
-- Not found: unknown
-- Found: good
-- Not found: bad
-- [False,True,False]

-- | So, how to stop a traverse when a condition is met?
-- With MonadPlus we can encode a stopping condition:
-- https://hackage.haskell.org/package/base-4.16.0.0/docs/Control-Monad.html#t:MonadPlus
stopOnTrue :: IO Bool -> MaybeT IO ()
stopOnTrue action = do
  result <- lift action
  when result mzero

-- | Which can be used like this:
-- >>> runMaybeT $ traverse (stopOnTrue . validUser) ["unknown", "good", "bad"]
-- Not found: unknown
-- Found: good
-- Nothing
--
-- >>> runMaybeT $ traverse (stopOnTrue . validUser) ["unknown", "bad"]
-- Not found: unknown
-- Not found: bad
-- Just [(),()]
--
-- Note that in our context, `Nothing` means the user is valid in a workspace,
-- and any `Just` value would means the user is invalid.

--
--
-- In summary, by wrapping the action inside a MaybeT, we gain a new ability
-- to control the evaluation. For example, by using mzero, we can abort the
-- computation.

-- Bonus: A generic point free version of stopOnTrue
stopOnTrue' :: (MonadTrans t, MonadPlus (t m), Monad m) => m Bool -> t m ()
stopOnTrue' = lift >=> bool mzero (pure ())

-- Bonus2: An helper to make the call site cleaner
-- >>> traverseUntil validUser ["unknown", "good", "bad"]
-- Not found: unknown
-- True
traverseUntil :: Monad m => (a -> m Bool) -> [a] -> m Bool
traverseUntil check xs = do
  result <- runMaybeT $ traverse (stopOnTrue' . check) xs
  case result of
    Nothing -> pure True
    Just _ -> pure False
