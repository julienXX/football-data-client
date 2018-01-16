{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Client where

import Control.Monad.Trans.Reader
import Data.Proxy
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Servant.API
import Servant.Client
import System.Environment (getEnv)

import Competition
import Team
import Player
import Fixture
import CompetitionFixture
import CompetitionTable

type API =
       "competitions"
       :> Get '[JSON] [Competition]
  :<|> "competitions"
       :> Capture "id" Int
       :> Get '[JSON] Competition
  :<|> "competitions"
       :> Capture "competition_id" Int
       :> "fixtures"
       :> Get '[JSON] CompetitionFixtures
  :<|> "competitions"
       :> Capture "competition_id" Int
       :> "leagueTable"
       :> Get '[JSON] CompetitionTable
  :<|> "teams"
       :> Capture "id" Int
       :> Get '[JSON] Team
  :<|> "teams"
       :> Capture "team_id" Int
       :> "players"
       :> Get '[JSON] TeamPlayers
  :<|> "fixtures"
       :> Capture "id" Int
       :> Get '[JSON] Fixture

baseUrl :: BaseUrl
baseUrl = BaseUrl Http "api.football-data.org" 80 "/v1"

footballDataApi :: Proxy API
footballDataApi = Proxy

getCompetitions :: ClientM [Competition]
getCompetition :: Int -> ClientM Competition
getCompetitionFixtures :: Int -> ClientM CompetitionFixtures
getCompetitionTable :: Int -> ClientM CompetitionTable
getTeam :: Int -> ClientM Team
getTeamPlayers :: Int -> ClientM TeamPlayers
getFixture :: Int -> ClientM Fixture
getCompetitions
  :<|> getCompetition
  :<|> getCompetitionFixtures
  :<|> getCompetitionTable
  :<|> getTeam
  :<|> getTeamPlayers
  :<|> getFixture
  = client footballDataApi

type FootballDataClient a = ClientM a

runClient' :: FootballDataClient a -> ClientEnv -> IO (Either ServantError a)
runClient' fcm = runClientM fcm

runClient :: FootballDataClient a -> Manager -> BaseUrl -> IO (Either ServantError a)
runClient fcm manager url = runClient' fcm (ClientEnv manager url)

runFootClient :: Manager -> FootballDataClient a -> IO (Either ServantError a)
runFootClient manager fcm = runClient fcm manager baseUrl

exampleQuery :: IO ()
exampleQuery = do
  m <- newManager defaultManagerSettings
  res <- runFootClient m $ do
    c <- getCompetitions
    let links = Competition._links $ head c
    pure $ links
  print res
