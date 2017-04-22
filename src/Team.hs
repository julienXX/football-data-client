{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Team where

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client


data Team = Team
  { name :: String
  , code :: String
  , shortName :: String
  , squadMarketValue :: String
  , crestUrl :: String
  , _links :: Links
  } deriving (Show, Generic)

instance FromJSON Team

data Links = Links
  { self :: Link
  , fixtures :: Link
  , players :: Link
  } deriving (Show, Generic)

instance FromJSON Links

data Link = Link
  { href :: String
  } deriving (Show, Generic)

instance FromJSON Link
