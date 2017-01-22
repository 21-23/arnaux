module Network.WebSocketServer where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Prelude (Unit)

foreign import data WS :: !
foreign import data WebSocketServer :: *

foreign import data Connection :: *
type Message = String

foreign import create :: ∀ e. Int -> Eff (ws :: WS | e) WebSocketServer

foreign import acceptConnection :: ∀ e. WebSocketServer -> (Connection -> Eff (ws :: WS | e) Unit) -> Eff (ws :: WS | e) Unit

foreign import logConnection :: ∀ e. Connection -> Eff (console :: CONSOLE | e) Unit
foreign import logMessage :: ∀ e. Message -> Eff (console :: CONSOLE | e) Unit

foreign import receiveMessage :: ∀ e. Connection -> (Message -> Eff (ws :: WS | e) Unit) -> Eff (ws :: WS | e) Unit

foreign import sendMessage :: ∀ e. Connection -> Message -> Eff (ws :: WS | e) Unit
