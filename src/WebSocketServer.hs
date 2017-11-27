{-# LANGUAGE OverloadedStrings #-}

module WebSocketServer (startServer) where

import           Control.Concurrent             (newMVar)
import           Data.Monoid                    ((<>))
import           Network.HTTP.Types             (status400)
import           Network.Wai                    (Application, responseLBS)
import qualified Network.Wai.Handler.Warp       as Warp
import           Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets             as WebSocket
import qualified System.Logger                  as Logger
import qualified System.Envy                    as Envy

import           ServerApplication              (application)
import qualified State
import           ServerApplication              (application)
import           Config                         (Config(Config))

backupApp :: Application
backupApp _ respond =
  respond $ responseLBS status400 [] "Not a WebSocket request"

startServer :: IO ()
startServer = do
  envConfig <- Envy.decodeEnv :: IO (Either String Config)
  case envConfig of
    Left err -> putStrLn $ "Config error:" <> err
    Right (Config port _) -> do
      let infoMessage = "🕊  Listening on port " <> show port
      putStrLn infoMessage
      stateVar <- newMVar State.empty
      logger <- Logger.create $ Logger.Path "logs/arnaux.log"
      Logger.info logger $ Logger.msg infoMessage
      Warp.run port $ websocketsOr WebSocket.defaultConnectionOptions
                                   (application stateVar)
                                   backupApp
