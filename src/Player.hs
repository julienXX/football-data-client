{-# LANGUAGE DeriveGeneric #-}

module Player where

import Data.Aeson
import GHC.Generics

data TeamPlayers = TeamPlayers
  { _links :: Links
  , count :: Int
  , players :: [Player]
  } deriving (Show, Generic)

data Player = Player
  { name :: String
  , position :: String
  , jerseyNumber :: Int
  , dateOfBirth :: String
  , nationality :: String
  , contractUntil :: String
  } deriving (Show, Generic)

data Links = Links
  { self :: Link
  , team :: Link
  } deriving (Show, Generic)

data Link = Link
  { href :: String
  } deriving (Show, Generic)

instance FromJSON TeamPlayers
instance FromJSON Player
instance FromJSON Links
instance FromJSON Link
