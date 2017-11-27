{-# OPTIONS_GHC -fno-warn-orphans #-}

module Config where

import           Network.Wai.Handler.Warp (Port)
import           System.Envy              (FromEnv (fromEnv),
                                           Var (fromVar, toVar), envMaybe,
                                           (.!=))
import           System.Logger            (Level (Debug))
import           Text.Read                (readMaybe)

data Config = Config
  { port     :: Port
  , logLevel :: Level
  } deriving (Show)

instance Var Level where
  toVar = show
  fromVar = readMaybe

instance FromEnv Config where
  fromEnv =
    Config <$> envMaybe "ARNAUX_PORT"      .!= 3000
           <*> envMaybe "ARNAUX_LOG_LEVEL" .!= Debug
