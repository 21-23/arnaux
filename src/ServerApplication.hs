{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module ServerApplication (application) where

import           Data.Text                 (Text, pack)
import qualified Data.Text.IO              as TextIO
import           Control.Concurrent        (MVar, putMVar, takeMVar)
import           Control.Monad             (forever)
import           Data.ByteString.Lazy      (ByteString)
import           Data.Monoid               ((<>))
import qualified Data.Map.Strict           as Map
import qualified Network.WebSockets        as WebSocket
import           Data.Aeson                (eitherDecode)
import           Control.Exception         (catch)
import qualified Data.UUID.V4              as UUID

import           State                     ( Connection (Connection)
                                           , ConnectionState (Accepted, CheckedIn)
                                           , State (State, connections, clients)
                                           )
import qualified State
import           Envelope                  (Envelope(Envelope))
import           Identity (Identity)
import           Message                   (IncomingMessage(CheckIn, Message))

data Action
  = Connect Connection
  | Disconnect Connection
  | Incoming Connection (Envelope IncomingMessage) ByteString

data Effect
  = Log Text
  | Send Connection ByteString

data MessageError
  = IdentityAlreadyCheckedIn Identity
  | IdentityNotCheckedIn Identity
  | InconsistentState
  | MessageBeforeCheckIn
  | RepeatedCheckIn
  deriving (Show)

stateLogic :: Action -> State -> Either MessageError (State, Effect)
stateLogic (Connect connection) state =
  let newState = State.connect connection state
      effect   = Log $ "New connection: " <> pack (show connection)
   in Right (newState, effect)

stateLogic (Incoming connection (Envelope _ (CheckIn identity)) _) state@State {connections, clients} =
  case Map.lookup connection connections of
    Nothing            -> Left InconsistentState
    Just (CheckedIn _) -> Left RepeatedCheckIn
    Just Accepted      -> case Map.lookup identity clients of
                            Just _  -> Left $ IdentityAlreadyCheckedIn identity
                            Nothing ->
                              let newState = State.checkIn connection identity state
                                  effect   = Log $ "CheckIn: " <> pack (show identity)
                               in Right (newState, effect)

stateLogic (Incoming connection (Envelope identity Message) messageString) state@State {connections, clients} =
  case Map.lookup connection connections of
    Nothing            -> Left InconsistentState
    Just Accepted      -> Left MessageBeforeCheckIn
    Just (CheckedIn _) -> case Map.lookup identity clients of
                            Nothing     -> Left $ IdentityNotCheckedIn identity
                            Just client -> Right (state, Send client messageString)

stateLogic (Disconnect connection) state@State {connections} =
  let newState = State.disconnect connection state
   in case Map.lookup connection connections of
        Just Accepted ->
          Right (newState, Log $ "Disconnected: " <> pack (show connection))
        Just (CheckedIn identity) ->
          Right (newState, Log $ "Checked out & disconnected: " <> pack (show identity))
        Nothing -> Left InconsistentState

handleEffect :: Effect -> IO ()
handleEffect (Log string) = TextIO.putStrLn $ "🕊  " <> string
handleEffect (Send (Connection _ connection) string) =
  WebSocket.sendTextData connection string

application :: MVar State -> WebSocket.ServerApp
application stateVar pending = do
  wsConnection <- WebSocket.acceptRequest pending
  WebSocket.forkPingThread wsConnection 30 --seconds

  connectionId <- UUID.nextRandom
  state_ <- takeMVar stateVar
  putMVar stateVar $ State.connect (Connection connectionId wsConnection) state_

  catch (forever
    (do
      string <- WebSocket.receiveData wsConnection :: IO ByteString
      case eitherDecode string :: Either String (Envelope IncomingMessage) of
        Right envelope -> do
          state <- takeMVar stateVar
          case stateLogic (Incoming (Connection connectionId wsConnection) envelope string) state of
            Left err -> do
              putMVar stateVar state
              print err
            Right (newState, effect) -> do
              putMVar stateVar newState
              handleEffect effect
        Left err -> TextIO.putStrLn $ "Message parsing error " <> pack err))
    (\e -> do
        TextIO.putStrLn $ pack $ show (e :: WebSocket.ConnectionException)
        return ())
