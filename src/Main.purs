module Main where

import Prelude
import Network.WebSocketServer
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Rec.Class (forever)
import Control.Monad.ST (ST, STRef, modifySTRef, newSTRef, readSTRef)
import Data.Argonaut (jsonParser, decodeJson, encodeJson)
import Data.Argonaut.Core (stringify)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, empty, insert, lookup)
import Messages (IncomingMessage(..))

messageHandler :: ∀ e h. STRef h (StrMap Connection) -> Connection -> Message -> Eff (console :: CONSOLE, st :: ST h, ws :: WS | e) Unit
messageHandler state connection message = do
  case jsonParser message >>= decodeJson of
    Left error -> log error
    Right msg -> do
      case msg of
        CheckIn {name} -> do
          log $ "checkin: " <> name
          modifySTRef state \connections ->
            insert name connection connections
          pure unit
        Message {to} -> do
          log $ "to: " <> to
          connections <- readSTRef state
          case lookup to connections of
            Nothing -> log $ to <> " is not connected"
            Just toConnection -> do
              sendMessage toConnection message
          pure unit
  receiveMessage connection $ messageHandler state connection

connectionHandler :: ∀ e h. WebSocketServer -> STRef h (StrMap Connection) -> Connection -> Eff (console :: CONSOLE, st :: ST h, ws :: WS | e) Unit
connectionHandler server state connection = do
  receiveMessage connection $ messageHandler state connection
  acceptConnection server $ connectionHandler server state

main :: forall e h. Eff (console :: CONSOLE, st :: ST h, ws :: WS | e) Unit
main = do
  state <- newSTRef empty
  server <- create 3000
  acceptConnection server $ connectionHandler server state
