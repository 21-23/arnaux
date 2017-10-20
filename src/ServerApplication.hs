{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module ServerApplication (application) where

import           Data.ByteString.Lazy (ByteString)
import           Data.Monoid          ((<>))
import           Control.Concurrent   (MVar, takeMVar, putMVar)
import           Control.Monad        (forever)
-- import qualified Data.Map.Strict       as Map
import qualified Network.WebSockets    as WebSocket
-- import           Data.Aeson           (eitherDecode)
import qualified Data.UUID.V4          as UUID
import           Control.Exception    (IOException, catch)

import           State                (State, ConnectionState(Accepted))
import qualified State
-- import           Envelope             (Envelope(Envelope, message, to))
-- import           Message              (IncomingMessage(Checkin, Message))

application :: MVar State -> WebSocket.ServerApp
application stateVar pending = do
  connection <- WebSocket.acceptRequest pending
  WebSocket.forkPingThread connection 30 --seconds

  connectionId <- UUID.nextRandom
  state <- takeMVar stateVar
  putMVar stateVar $ State.addConnection (Accepted connection) connectionId state
  print $ "Joined: " <> show connectionId

  forever $ catch
    (do
      string <- WebSocket.receiveData connection :: IO ByteString
      print string)
    (\e -> do
        print (e :: IOException)
        return ())
    -- case eitherDecode string :: Either String (Envelope IncomingMessage) of
    --   Right Envelope { message, to } -> case message of
    --     Checkin identity -> do
    --       state <- takeMVar stateVar
    --       putMVar stateVar $ State.addClient connection identity state
    --       putStrLn $ "🕊  Checkin: " <> show identity
    --     Message -> do
    --       (State clients) <- readMVar stateVar
    --       case Map.lookup to clients of
    --         Just recipient -> WebSocket.sendTextData recipient string
    --         Nothing        -> return ()
    --
    --   Left err -> putStrLn $ "Message parsing error " <> err
