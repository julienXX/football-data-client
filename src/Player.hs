{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Player where

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client

data TeamPlayers = TeamPlayers
  { _links :: Links
  , count :: Int
  , players :: [Player]
  } deriving (Show, Generic)

instance FromJSON TeamPlayers

data Player = Player
  { name :: String
  , position :: String
  , jerseyNumber :: Int
  , dateOfBirth :: String
  , nationality :: String
  , contractUntil :: String
  } deriving (Show, Generic)

instance FromJSON Player

data Links = Links
  { self :: Link
  , team :: Link
  } deriving (Show, Generic)

instance FromJSON Links

data Link = Link
  { href :: String
  } deriving (Show, Generic)

instance FromJSON Link
