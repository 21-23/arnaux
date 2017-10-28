{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module ServerApplication (application) where

import           Control.Concurrent        (MVar, putMVar, takeMVar)
import           Control.Monad             (forever)
import           Data.ByteString.Lazy      (ByteString)
import           Data.Monoid               ((<>))
import qualified Data.Map.Strict       as Map
import qualified Network.WebSockets        as WebSocket
import           Data.Aeson           (eitherDecode)
import           Control.Exception         (catch)
import qualified Data.UUID.V4              as UUID

import           State                     (ConnectionState (Accepted), State(State, clients))
import qualified State
import           Envelope             (Envelope(Envelope))
import           Identity (Identity)
import           Message                   (IncomingMessage(CheckIn, Message))

-- data ConnectionStateError
--   = MessageBeforeCheckIn
--   | RepeatedCheckIn

data Effect
  = None
  | Send WebSocket.Connection ByteString

data MessageError
  = IdentityAlreadyCheckedIn Identity
  | IdentityNotCheckedIn Identity
  deriving (Show)

data Incoming = Incoming (Envelope IncomingMessage) ByteString

-- cStateLogic :: ConnectionState -> IncomingMessage -> Either ConnectionStateError ConnectionState
-- cStateLogic (Accepted connection) (CheckIn identity) = Right $ CheckedIn connection identity
-- cStateLogic (Accepted _)           _                 = Left MessageBeforeCheckIn
-- cStateLogic (CheckedIn _ _)       (CheckIn _)        = Left RepeatedCheckIn
-- cStateLogic state                  _                 = Right state

stateLogic :: WebSocket.Connection -> Incoming -> State -> Either MessageError (State, Effect)
stateLogic connection (Incoming (Envelope _ (CheckIn identity)) _) state@State {clients} =
  case Map.lookup identity clients of
    Just _  -> Left $ IdentityAlreadyCheckedIn identity
    Nothing -> Right (State.addClient connection identity state, None)

stateLogic _ (Incoming (Envelope identity Message) messageString) state@State {clients} =
  case Map.lookup identity clients of
    Nothing     -> Left $ IdentityNotCheckedIn identity
    Just client -> Right (state, Send client messageString)

handleEffect :: Effect -> IO ()
handleEffect None = return ()
handleEffect (Send connection string) = WebSocket.sendTextData connection string

application :: MVar State -> WebSocket.ServerApp
application stateVar pending = do
  connection <- WebSocket.acceptRequest pending
  WebSocket.forkPingThread connection 30 --seconds

  connectionId <- UUID.nextRandom
  state_ <- takeMVar stateVar
  putMVar stateVar $ State.addConnection (Accepted connection) connectionId state_
  putStrLn $ "Joined: " <> show connectionId

  catch (forever
    (do
      string <- WebSocket.receiveData connection :: IO ByteString
      putStrLn $ "Received: " <> show string <> " from " <> show connectionId
      case eitherDecode string :: Either String (Envelope IncomingMessage) of
        Right envelope -> do
          state <- takeMVar stateVar
          case stateLogic connection (Incoming envelope string) state of
            Left err -> do
              putMVar stateVar state
              print err
            Right (newState, effect) -> do
              putMVar stateVar newState
              handleEffect effect
        Left err -> putStrLn $ "Message parsing error " <> err))
    (\e -> do
        putStrLn $ "Disconnected: " <> show connectionId <> show (e :: WebSocket.ConnectionException)
        return ())
