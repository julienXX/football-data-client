{-# LANGUAGE DeriveGeneric #-}

module Team where

import Data.Aeson
import GHC.Generics

data Team = Team
  { name :: String
  , code :: String
  , shortName :: String
  , squadMarketValue :: String
  , crestUrl :: String
  , _links :: Links
  } deriving (Show, Generic)

data Links = Links
  { self :: Link
  , fixtures :: Link
  , players :: Link
  } deriving (Show, Generic)

data Link = Link
  { href :: String
  } deriving (Show, Generic)

instance FromJSON Team
instance FromJSON Links
instance FromJSON Link
