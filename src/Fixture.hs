{-# LANGUAGE DeriveGeneric #-}

module Fixture where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics

data DetailedFixture = DetailedFixture
  { fixture :: DetailedFixture
  , head2head :: HeadToHead
  } deriving (Show, Generic)

instance FromJSON DetailedFixture

data Fixture = Fixture
  { date :: String
  , status :: String
  , matchday :: Int
  , homeTeamName :: String
  , awayTeamName :: String
  , result :: FixtureResult
  , _links :: Links
  } deriving (Show, Generic)

instance FromJSON Fixture

data FixtureResult = FixtureResult
  { goalsHomeTeam :: Maybe Int
  , goalsAwayTeam :: Maybe Int
  , halfTime :: Maybe DetailedGoals
  , extraTime :: Maybe DetailedGoals
  , penaltyShootout :: Maybe DetailedGoals
  } deriving (Show, Generic)

instance FromJSON FixtureResult

data HeadToHead = HeadToHead
  { count :: Int
  , timeFrameStart :: String
  , timeFrameEnd :: String
  , homeTeamWins :: Int
  , awayTeamWins :: Int
  , draws :: Int
  , lastHomeWinHomeTeam :: DetailedFixture
  , lastWinHomeTeam :: DetailedFixture
  , lastAwayWinAwayTeam :: DetailedFixture
  , lastWinAwayTeam :: DetailedFixture
  , fixtures :: [DetailedFixture]
  } deriving (Show, Generic)

instance FromJSON HeadToHead

data DetailedGoals = DetailedGoals
  { _goalsHomeTeam :: Int
  , _goalsAwayTeam :: Int
  } deriving (Show, Generic)

instance FromJSON DetailedGoals where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 1 }

data Links = Links
  { self :: Link
  , competition :: Link
  , homeTeam :: Link
  , awayTeam :: Link
  } deriving (Show, Generic)

instance FromJSON Links

data Link = Link
  { href :: String
  } deriving (Show, Generic)

instance FromJSON Link
