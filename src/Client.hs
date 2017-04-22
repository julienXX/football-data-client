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


type API =
       "competitions" :> Capture "id" Int :> Get '[JSON] Competition
  :<|> "teams" :> Capture "id" Int :> Get '[JSON] Team

baseUrl :: BaseUrl
baseUrl = BaseUrl Http "api.football-data.org" 80 "/v1"

footballDataApi :: Proxy API
footballDataApi = Proxy

getCompetition :: Int -> ClientM Competition
getTeam :: Int -> ClientM Team
getCompetition :<|> getTeam = client footballDataApi

run :: Int -> IO ()
run id = do
  m <- newManager defaultManagerSettings
  res <- runClientM (getTeam id) (ClientEnv m baseUrl)
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right object -> do
      print object
