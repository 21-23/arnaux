module State where

import Prelude
import Data.Map as Map
import Data.StrMap as StrMap
import Data.Maybe (Maybe(..))
import Network.WebSocketServer (Connection)

data State = State
  { identities :: Map.Map Connection String
  , connections :: StrMap.StrMap Connection
  }

empty :: State
empty = State { identities: Map.empty, connections: StrMap.empty }

checkIn :: Connection -> String -> State -> State
checkIn connection identity (State state@{identities, connections}) = State $ state
  { identities = Map.insert connection identity identities
  , connections = StrMap.insert identity connection connections }

checkOut :: Connection -> State -> State
checkOut connection (State state@{identities, connections}) =
  case Map.lookup connection identities of
    Nothing -> State state
    Just identity -> State $ state
      { identities = Map.delete connection identities
      , connections = StrMap.delete identity connections }
