{-# LANGUAGE DeriveGeneric #-}

module CompetitionFixture where

import Data.Aeson
import GHC.Generics
import Fixture (Fixture)

data CompetitionFixtures = CompetitionFixtures
  { count :: Int
  , fixtures :: [Fixture]
  , _links :: Links
  } deriving (Show, Generic)

data Links = Links
  { self :: Link
  , competition :: Link
  } deriving (Show, Generic)

data Link = Link
  { href :: String
  } deriving (Show, Generic)

instance FromJSON CompetitionFixtures
instance FromJSON Links
instance FromJSON Link
