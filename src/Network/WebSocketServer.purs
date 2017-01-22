module Network.WebSocketServer where

import Control.Monad.Eff (Eff)
import Data.Ord.Unsafe (unsafeCompare)
import Prelude (class Eq, class Ord, Unit)

foreign import data WS :: !
foreign import data WebSocketServer :: *

foreign import data Connection :: *

foreign import connectionEq :: Connection -> Connection -> Boolean

instance eqConnection :: Eq Connection where
  eq = connectionEq

instance ordConnection :: Ord Connection where
  compare = unsafeCompare

type Message = String

foreign import create
  :: ∀ e
   . Int
  -> Eff (ws :: WS | e) WebSocketServer

foreign import acceptConnection
  :: ∀ e
   . WebSocketServer
  -> (Connection -> Eff (ws :: WS | e) Unit)
  -> Eff (ws :: WS | e) Unit

foreign import closeConnection
  :: ∀ e
   . Connection
  -> (Connection -> Eff (ws :: WS | e) Unit)
  -> Eff (ws :: WS | e) Unit

foreign import receiveMessage
  :: ∀ e
   . Connection
  -> (Message -> Eff (ws :: WS | e) Unit)
  -> Eff (ws :: WS | e) Unit

foreign import sendMessage
  :: ∀ e
   . Connection
  -> Message
  -> Eff (ws :: WS | e) Unit
