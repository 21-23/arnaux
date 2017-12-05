{-# LANGUAGE OverloadedStrings #-}

module ServerApplication (application) where

import           Control.Concurrent               (MVar, putMVar, takeMVar)
import           Control.Exception                (catch)
import           Control.Monad                    (forever)
import qualified Control.Monad.Trans.State.Strict as StateT
import           Data.Aeson                       (eitherDecode)
import           Data.ByteString.Lazy             (ByteString)
import           Data.Functor                     (($>))
import           Data.Monoid                      ((<>))
import           Data.Text                        (pack)
import qualified Data.UUID.V4                     as UUID
import qualified Network.WebSockets               as WebSocket
import           System.Logger                    (Logger, Level(Info, Warn))
import qualified System.Logger                    as Logger


import           Connection                       (Connection (Connection), ConnectionState (Accepted, CheckedIn))
import           Effect                           (Effect (Log, Send), handle)
import           Envelope                         (Envelope (Envelope))
import           Message                          (IncomingMessage (CheckIn, CheckOut, Message))
import           Query                            (failure, success)
import           State                            (State)
import qualified State
import           StateQuery                       (ServiceError (CheckOutWrongIdentity),
                                                   StateQuery)
import           Validation                       (connected,
                                                   connectionCanCheckOut,
                                                   connectionCanMessage,
                                                   identityIsAvailable,
                                                   identityIsCheckedIn,
                                                   notCheckedInYet)

data Action
  = Connect Connection
  | Disconnect Connection WebSocket.ConnectionException
  | Incoming Connection (Envelope IncomingMessage) ByteString

stateLogic :: StateQuery Action Effect
stateLogic (Connect connection) = update $> effect
  where
    update = StateT.modify $ State.connect connection
    effect = Log Info $ "New connection: " <> pack (show connection)

stateLogic (Incoming connection (Envelope _ (CheckIn identity)) _) = do
  connectionState <- connected connection
  notCheckedInYet connectionState
  identityIsAvailable identity
  StateT.modify $ State.checkIn connection identity
  success $ Log Info $ "CheckIn: "
                    <> pack (show identity)
                    <> " "
                    <> pack (show connection)

stateLogic (Incoming connection (Envelope _ (CheckOut identity)) _) = do
  connectionState <- connected connection
  checkedInIdentity <- connectionCanCheckOut connectionState
  _ <- identityIsCheckedIn identity
  if checkedInIdentity /= identity
    then failure $ CheckOutWrongIdentity identity checkedInIdentity
    else do
      StateT.modify $ State.checkOut identity
      success $ Log Info $ "CheckOut: "
                        <> pack (show identity)
                        <> " "
                        <> pack (show connection)

stateLogic (Incoming connection (Envelope identity Message) messageString) = do
  connectionState <- connected connection
  connectionCanMessage connectionState
  client <- identityIsCheckedIn identity
  success $ Send client messageString

stateLogic (Disconnect connection exception) = do
  connectionState <- connected connection
  let effect = case connectionState of
                Accepted           -> Log Info $ "Disconnected: "
                                              <> pack (show connection)
                                              <> " "
                                              <> pack (show exception)

                CheckedIn identity -> Log Info $ "Checked out & disconnected: "
                                              <> pack (show identity)
                                              <> " "
                                              <> pack (show exception)
  success effect

updateState :: Logger -> MVar State -> Action -> IO ()
updateState logger stateVar action = do
  state <- takeMVar stateVar
  case StateT.runStateT (stateLogic action) state of
    Left err -> do
      putMVar stateVar state
      handle logger $ Log Warn $ pack $ show err
    Right (effect, newState) -> do
      putMVar stateVar newState
      handle logger effect

application :: Logger -> MVar State -> WebSocket.ServerApp
application logger stateVar pending = do
  wsConnection <- WebSocket.acceptRequest pending
  WebSocket.forkPingThread wsConnection 30 --seconds

  connectionId <- UUID.nextRandom
  let connection = Connection connectionId wsConnection
  updateState logger stateVar (Connect connection)

  catch (forever
    (do
      string <- WebSocket.receiveData wsConnection :: IO ByteString
      Logger.trace logger $ Logger.msg $ "🕊  Received message: " <> string
      case eitherDecode string :: Either String (Envelope IncomingMessage) of
        Right envelope ->
          let action = Incoming connection envelope string
           in updateState logger stateVar action
        Left err -> Logger.warn logger $ Logger.msg $ "🕊  Message parsing error: " <> pack err))
    (updateState logger stateVar . Disconnect connection)
