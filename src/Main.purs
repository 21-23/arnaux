module Main where

import Prelude
import Network.WebSocketServer
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.ST (ST, STRef, modifySTRef, newSTRef, readSTRef)
import Data.Argonaut (jsonParser, decodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.StrMap (lookup)
import Messages (IncomingMessage(..))
import State (State(..), checkIn, checkOut, empty)

messageHandler
  :: ∀ e h
   . STRef h State
  -> Connection
  -> Message
  -> Eff (console :: CONSOLE, st :: ST h, ws :: WS | e) Unit
messageHandler state connection message = do
  case jsonParser message >>= decodeJson of
    Left error -> log error
    Right msg -> do
      case msg of
        CheckIn {identity} -> do
          log $ "checkin: " <> identity
          modifySTRef state $ checkIn connection identity
          pure unit
        Message {to} -> do
          log $ "to: " <> to
          State {connections} <- readSTRef state
          case lookup to connections of
            Nothing -> log $ to <> " is not connected"
            Just toConnection -> do
              sendMessage toConnection message
          pure unit
  receiveMessage connection $ messageHandler state connection

connectionCloseHandler
  :: ∀ e h
   . STRef h State
  -> Connection
  -> Eff (st :: ST h | e) Unit
connectionCloseHandler state connection = do
  modifySTRef state $ checkOut connection
  pure unit

connectionHandler
  :: ∀ e h
   . WebSocketServer
  -> STRef h State
  -> Connection
  -> Eff (console :: CONSOLE, st :: ST h, ws :: WS | e) Unit
connectionHandler server state connection = do
  receiveMessage connection $ messageHandler state connection
  closeConnection connection $ connectionCloseHandler state
  acceptConnection server $ connectionHandler server state

main :: forall e h. Eff (console :: CONSOLE, st :: ST h, ws :: WS | e) Unit
main = do
  state <- newSTRef $ empty
  server <- create 3000
  acceptConnection server $ connectionHandler server state
