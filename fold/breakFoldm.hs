-- |
module Main where

import Data.Foldable
import Data.Monoid

xs :: [Int]
xs = [1, 4, 2, 52, 3]

getFirstHigherTen :: [Int] -> Maybe Int
getFirstHigherTen = getFirst . foldMap go
  where
    go :: Int -> First Int
    go x
      | x > 10 = First $ Just x
      | otherwise = First Nothing

main = print $ getFirstHigherTen xs
