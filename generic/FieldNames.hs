{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Data
import Data.Int
import Data.Proxy
import GHC.Generics

-- | A typeclass to extract the field (selectors) name of a record
-- adapted from https://stackoverflow.com/a/27818445
class Selectors rep where
  selectors :: Proxy rep -> [String]

-- | Create instances for records
instance Selectors f => Selectors (M1 D x f) where
  selectors _ = selectors (Proxy :: Proxy f)

instance Selectors f => Selectors (M1 C x f) where
  selectors _ = selectors (Proxy :: Proxy f)

instance (Selector s, Typeable t) => Selectors (M1 S s (K1 R t)) where
  selectors _ =
    [(selName (undefined :: M1 S s (K1 R t) ()))]

instance (Selectors a, Selectors b) => Selectors (a :*: b) where
  selectors _ = selectors (Proxy :: Proxy a) ++ selectors (Proxy :: Proxy b)

instance Selectors U1 where
  selectors _ = []

data EChangeEvent = ECE {eventName :: String, eventId :: Int}
  deriving (Generic)

data EChangeSmall = ECS {changeName :: String, changeId :: Int, changeAuthor :: String}
  deriving (Generic)

searchField :: forall a. (Selectors (Rep a)) => IO [a]
searchField = do
  putStrLn $ "Running query for fields: " <> show (selectors (Proxy :: Proxy (Rep a)))
  pure []

main :: IO ()
main = do
  _ <- searchField :: IO [EChangeSmall]
  _ <- searchField :: IO [EChangeSmall]
  pure ()

-- |
-- > searchField :: IO [Bool]
--
-- <interactive>:19:1: error:
--     • No instance for (Selectors
--                          (C1 ('MetaCons "False" 'PrefixI 'False) U1
--                           :+: C1 ('MetaCons "True" 'PrefixI 'False) U1))
--         arising from a use of ‘searchField’
--     • In the expression: searchField "toto" :: IO [Bool]
--       In an equation for ‘it’: it = searchField "toto" :: IO [Bool]
