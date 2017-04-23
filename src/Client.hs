{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Client where

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client

import Competition
import Team
import Player


type API =
       "competitions" :> Get '[JSON] [Competition]
  :<|> "competitions" :> Capture "id" Int :> Get '[JSON] Competition
  :<|> "teams" :> Capture "id" Int :> Get '[JSON] Team
  :<|> "teams" :> Capture "team_id" Int :> "players" :> Get '[JSON] TeamPlayers

baseUrl :: BaseUrl
baseUrl = BaseUrl Http "api.football-data.org" 80 "/v1"

footballDataApi :: Proxy API
footballDataApi = Proxy

getCompetitions :: ClientM [Competition]
getCompetition :: Int -> ClientM Competition
getTeam :: Int -> ClientM Team
getTeamPlayers :: Int -> ClientM TeamPlayers
getCompetitions :<|> getCompetition :<|> getTeam :<|> getTeamPlayers = client footballDataApi

run :: Int -> IO ()
run id = do
  m <- newManager defaultManagerSettings
  res <- runClientM (getTeamPlayers id) (ClientEnv m baseUrl)
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right object -> do
      print object
