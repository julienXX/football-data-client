{-# LANGUAGE DeriveGeneric #-}

module CompetitionTable where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics

data CompetitionTable = CompetitionTable
  { matchday :: Int
  , leagueCaption :: String
  , standing :: [Standing]
  , _links :: Links
  } deriving (Show, Generic)

data Standing = Standing
  { position :: Int
  , teamName :: String
  , crestURI :: String
  , playedGames :: Int
  , points :: Int
  , goals :: Int
  , goalsAgainst :: Int
  , goalDifference :: Int
  , wins :: Int
  , draws :: Int
  , losses :: Int
  , home :: HomeAwayGoals
  , away :: HomeAwayGoals
  } deriving (Show, Generic)

data HomeAwayGoals = HomeAwayGoals
  { _goals :: Int
  , _goalsAgainst :: Int
  , _wins :: Int
  , _draws :: Int
  , _losses :: Int
  } deriving (Show, Generic)

instance FromJSON HomeAwayGoals where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 1 }

data Links = Links
  { self :: Link
  , competition :: Link
  } deriving (Show, Generic)

data Link = Link
  { href :: String
  } deriving (Show, Generic)

instance FromJSON CompetitionTable
instance FromJSON Standing
instance FromJSON Links
instance FromJSON Link
