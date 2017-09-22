module Main where

import Client
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.Client

main :: IO ()
main = do
  m <- newManager defaultManagerSettings
  res <- runClientM (getTeam 86) (ClientEnv m baseUrl)
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right object -> do
      print object
