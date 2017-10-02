{-# LANGUAGE OverloadedStrings #-}

module WebSocketServer (startServer) where

import           Control.Concurrent             (newMVar)
import qualified Network.WebSockets              as WebSocket
import           Network.Wai                    (Application, responseLBS)
import qualified Network.Wai.Handler.Warp        as Warp
import           Network.Wai.Handler.WebSockets (websocketsOr)
import           Network.HTTP.Types             (status400)
import           Data.Monoid                    ((<>))

import qualified State
import           ServerApplication              (application)

backupApp :: Application
backupApp _ respond =
  respond $ responseLBS status400 [] "Not a WebSocket request"

startServer :: IO ()
startServer = do
  let port = 3000
  stateVar <- newMVar State.empty
  putStrLn $ "🕊  Listening on port " <> show port
  Warp.run port $ websocketsOr WebSocket.defaultConnectionOptions
                               (application stateVar)
                               backupApp
