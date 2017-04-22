{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Competition where

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client


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

instance FromJSON Competition

data Links = Links
  { self :: Link
  , teams :: Link
  , fixtures :: Link
  , leagueTable :: Link
  } deriving (Show, Generic)

instance FromJSON Links

data Link = Link
  { href :: String
  } deriving (Show, Generic)

instance FromJSON Link
