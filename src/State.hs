{-# LANGUAGE NamedFieldPuns #-}

module State where

import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict     as Map
import           Network.WebSockets (Connection)
import           Data.UUID          (UUID)

import           Identity           (Identity)

type ConnectionId = UUID

data ConnectionState
  = Accepted Connection
  | CheckedIn Connection Identity

data State = State
  { connections :: Map ConnectionId ConnectionState
  , clients :: Map Identity Connection
  }

empty :: State
empty = State
  { connections = Map.empty
  , clients     = Map.empty
  }

addConnection :: ConnectionState -> ConnectionId -> State -> State
addConnection connectionState connectionId state@(State connections _) =
  state { connections = Map.insert connectionId connectionState connections }

addClient :: Connection -> Identity -> State -> State
addClient connection identity state@(State _ clients) =
  state { clients = Map.insert identity connection clients }
--
-- removeClient :: Identity -> State -> State
-- removeClient identity state@(State clients) =
--   state { clients = Map.delete identity clients }
