{-# LANGUAGE DeriveGeneric #-}

module Competition where

import Data.Aeson
import GHC.Generics

data Competition = Competition
  { id :: Int
  , caption :: String
  , league :: String
  , year :: String
  , currentMatchday :: Int
  , numberOfMatchdays :: Int
  , numberOfTeams :: Int
  , numberOfGames :: Int
  , lastUpdated :: String
  , _links :: Links
  } deriving (Show, Generic)

data Links = Links
  { self :: Link
  , teams :: Link
  , fixtures :: Link
  , leagueTable :: Link
  } deriving (Show, Generic)

data Link = Link
  { href :: String
  } deriving (Show, Generic)

instance FromJSON Competition
instance FromJSON Links
instance FromJSON Link
