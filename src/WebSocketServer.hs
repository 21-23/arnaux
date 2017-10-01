{-# LANGUAGE OverloadedStrings #-}

module WebSocketServer (startServer) where

import          Control.Concurrent             (newMVar)
import qualified Network.WebSockets              as WebSocket
import          Network.Wai                    (Application, responseLBS)
import qualified Network.Wai.Handler.Warp        as Warp
import          Network.Wai.Handler.WebSockets (websocketsOr)
import          Network.HTTP.Types             (status400)

import qualified State
import          ServerApplication              (application)

backupApp :: Application
backupApp _ respond =
  respond $ responseLBS status400 [] "Not a WebSocket request"

startServer :: IO ()
startServer = do
  stateVar <- newMVar State.empty
  Warp.run 3000 $ websocketsOr WebSocket.defaultConnectionOptions
                               (application stateVar)
                               backupApp
