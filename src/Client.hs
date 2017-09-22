{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Client where

import Data.Proxy
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client

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

-- runQuery :: a -> Maybe Int -> IO ()
-- runQuery query id = do
--   m <- newManager defaultManagerSettings
--   res <- runClientM (query id) (ClientEnv m baseUrl)
--   case res of
--     Left err -> putStrLn $ "Error: " ++ show err
--     Right object -> do
--       print object
