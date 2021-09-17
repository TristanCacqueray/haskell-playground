-- |
module TP where

newtype ReadP a = R (String -> [(a, String)])

instance Functor ReadP where
  -- fmap :: (a -> b) -> ReadP a -> ReadP b
  fmap f (R r) = R $ \s ->
    -- res :: [(a, String)]
    let res = r s
        update (a, rest) = (f a, rest)
     in map update res

instance Applicative ReadP where
  -- pure :: a -> ReadP a
  pure x = R $ \s -> [(x, s)]

  -- <*> f (a -> b) -> f a -> f b
  (R rf) <*> (R ra) = R $ \s ->
    -- res :: [(a -> b, String)]
    let res = rf s
        (f, rest) : _ = res

        resa = ra rest
        (a, xs) : _ = resa
     in [(f a, xs)]

fakeGet :: ReadP Char
fakeGet = R $ \(x : xs) -> [(x, xs)]

runParser :: ReadP a -> String -> [(a, String)]
runParser (R r) = r
