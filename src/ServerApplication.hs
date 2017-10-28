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

import           Connection                ( Connection (Connection)
                                           , ConnectionState (Accepted, CheckedIn)
                                           )
import           State                     (State (State, connections, clients))
import qualified State
import           Envelope                  (Envelope(Envelope))
import           Identity (Identity)
import           Message                   (IncomingMessage(CheckIn, CheckOut, Message))

data Action
  = Connect Connection
  | Disconnect Connection WebSocket.ConnectionException
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
  | CheckOutWithoutCheckIn
  | CheckOutWrongIdentity Identity Identity
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
                                  effect   = Log $ "CheckIn: "
                                                    <> pack (show identity)
                                                    <> " "
                                                    <> pack (show connection)
                               in Right (newState, effect)

stateLogic (Incoming connection (Envelope _ (CheckOut identity)) _) state@State {connections, clients} =
  case Map.lookup connection connections of
    Nothing            -> Left InconsistentState
    Just Accepted      -> Left CheckOutWithoutCheckIn
    Just (CheckedIn cIdentity)
      | cIdentity /= identity -> Left $ CheckOutWrongIdentity identity cIdentity
      | otherwise             -> case Map.lookup identity clients of
                                   Nothing -> Left InconsistentState
                                   Just _ ->
                                     let newState = State.checkOut identity state
                                         effect   = Log $ "CheckOut: "
                                                          <> pack (show identity)
                                                          <> " "
                                                          <> pack (show connection)
                                      in Right (newState, effect)

stateLogic (Incoming connection (Envelope identity Message) messageString) state@State {connections, clients} =
  case Map.lookup connection connections of
    Nothing            -> Left InconsistentState
    Just Accepted      -> Left MessageBeforeCheckIn
    Just (CheckedIn _) -> case Map.lookup identity clients of
                            Nothing     -> Left $ IdentityNotCheckedIn identity
                            Just client -> Right (state, Send client messageString)

stateLogic (Disconnect connection exception) state@State {connections} =
  let newState = State.disconnect connection state
   in case Map.lookup connection connections of
        Just Accepted ->
          let effect
                 = Log $ "Disconnected: "
                <> pack (show connection)
                <> " "
                <> pack (show exception)
           in Right (newState, effect)
        Just (CheckedIn identity) ->
          let effect
                 = Log $ "Checked out & disconnected: "
                <> pack (show identity)
                <> " "
                <> pack (show exception)
           in Right (newState, effect)
        Nothing -> Left InconsistentState

handleEffect :: Effect -> IO ()
handleEffect (Log string) = TextIO.putStrLn $ "🕊  " <> string
handleEffect (Send (Connection _ connection) string) =
  WebSocket.sendTextData connection string

updateState :: MVar State -> Action -> IO ()
updateState stateVar action = do
  state <- takeMVar stateVar
  case stateLogic action state of
    Left err -> do
      putMVar stateVar state
      print err
    Right (newState, effect) -> do
      putMVar stateVar newState
      handleEffect effect

application :: MVar State -> WebSocket.ServerApp
application stateVar pending = do
  wsConnection <- WebSocket.acceptRequest pending
  WebSocket.forkPingThread wsConnection 30 --seconds

  connectionId <- UUID.nextRandom
  let connection = Connection connectionId wsConnection
  updateState stateVar (Connect connection)

  catch (forever
    (do
      string <- WebSocket.receiveData wsConnection :: IO ByteString
      case eitherDecode string :: Either String (Envelope IncomingMessage) of
        Right envelope ->
          let action = Incoming connection envelope string
           in updateState stateVar action
        Left err -> TextIO.putStrLn $ "Message parsing error " <> pack err))
    (updateState stateVar . Disconnect connection)
