module Tier0.Reader (Environment (..), EnvironmentM, formatUserName, formatHost, formatCurrentDir, formatPrompt) where

import Control.Monad.Reader

data Environment = Environment
  { username :: String
  , isSuperUser :: Bool
  , host :: String
  , currentDir :: String
  } deriving Eq

type EnvironmentM = Reader Environment

formatUserName :: EnvironmentM String
formatUserName = asks $ \env ->
  if isSuperUser env then "root" else username env
  
formatHost :: EnvironmentM String
formatHost = asks host

formatCurrentDir :: EnvironmentM String
formatCurrentDir = asks currentDir

formatPrompt :: EnvironmentM String
formatPrompt = do
  user <- formatUserName
  hostname <- formatHost
  dir <- formatCurrentDir
  return $ user ++ "@" ++ hostname ++ ":" ++ dir ++ "$"
