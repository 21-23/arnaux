{-# LANGUAGE OverloadedStrings #-}

module WebSocketServer (startServer) where

import           Control.Concurrent (newMVar)
import qualified Network.WebSockets  as WebSocket

import qualified State
import           ServerApplication  (application)

startServer :: IO ()
startServer = do
  stateVar <- newMVar State.empty
  WebSocket.runServer "127.0.0.1" 3000 $ application stateVar
