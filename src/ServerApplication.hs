{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module ServerApplication (application) where

import           Data.ByteString.Lazy (ByteString)
import           Data.Monoid          ((<>))
import           Control.Concurrent   (MVar, readMVar, takeMVar, putMVar)
import           Control.Monad        (forever)
import qualified Data.Map.Strict       as Map
import qualified Network.WebSockets    as WebSocket
import           Data.Aeson           (eitherDecode)

import           State                (State(State))
import qualified State
import           Envelope             (Envelope(Envelope, message))
import           Message              (IncomingMessage(Checkin, Message))

application :: MVar State -> WebSocket.ServerApp
application stateVar pending = do
  connection <- WebSocket.acceptRequest pending
  WebSocket.forkPingThread connection 30 --seconds
  forever $ do
    string <- WebSocket.receiveData connection :: IO ByteString
    print string
    case eitherDecode string :: Either String (Envelope IncomingMessage) of
      Right Envelope { message } -> case message of
        Checkin identity -> do
          state <- takeMVar stateVar
          putMVar stateVar $ State.addClient connection identity state
          putStrLn $ "🕊  Checkin: " <> show identity
        Message identity -> do
          (State clients) <- readMVar stateVar
          case Map.lookup identity clients of
            Just recipient -> WebSocket.sendTextData recipient string
            Nothing        -> return ()

      Left err -> putStrLn $ "Message parsing error " <> err
