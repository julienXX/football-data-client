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

type Token = String

type API =
       "competitions"
       :> Header "X-Auth-Token" Token
       :> Get '[JSON] [Competition]
  :<|> "competitions"
       :> Capture "id" Int
       :> Header "X-Auth-Token" Token
       :> Get '[JSON] Competition
  :<|> "competitions"
       :> Capture "competition_id" Int
       :> "fixtures"
       :> Header "X-Auth-Token" Token
       :> Get '[JSON] CompetitionFixtures
  :<|> "competitions"
       :> Capture "competition_id" Int
       :> "leagueTable"
       :> Header "X-Auth-Token" Token
       :> Get '[JSON] CompetitionTable
  :<|> "teams"
       :> Capture "id" Int
       :> Header "X-Auth-Token" Token
       :> Get '[JSON] Team
  :<|> "teams"
       :> Capture "team_id" Int
       :> "players"
       :> Header "X-Auth-Token" Token
       :> Get '[JSON] TeamPlayers
  :<|> "fixtures"
       :> Capture "id" Int
       :> Header "X-Auth-Token" Token
       :> Get '[JSON] Fixture

baseUrl :: BaseUrl
baseUrl = BaseUrl Http "api.football-data.org" 80 "/v1"

footballDataApi :: Proxy API
footballDataApi = Proxy

getCompetitions :: Token -> ClientM [Competition]
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

type FootballDataClient a = ReaderT Token ClientM a

runClient' :: FootballDataClient a -> Token -> ClientEnv -> IO (Either ServantError a)
runClient' fcm token = runClientM (runReaderT fcm token)

runClient :: FootballDataClient a -> Token -> Manager -> BaseUrl -> IO (Either ServantError a)
runClient fcm token manager url = runClient' fcm token (ClientEnv manager url)

runFootClient :: Token -> Manager -> FootballDataClient a -> IO (Either ServantError a)
runFootClient token manager fcm = runClient fcm token manager baseUrl

exampleQuery :: IO ()
exampleQuery = do
  token <- getEnv "FOOTBALL_DATA_TOKEN"
  m <- newManager defaultManagerSettings
  res <- runFootClient token m $ do
    c <- getCompetitions
    let links = Competition._links $ head c
    pure $ links
  print res
