{-# LANGUAGE NamedFieldPuns #-}

module State where

import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict     as Map
import           Network.WebSockets (Connection)

import           Identity           (Identity)

newtype State = State { clients :: Map Identity Connection }

empty :: State
empty = State
  { clients = Map.empty
  }

addClient :: Connection -> Identity -> State -> State
addClient connection identity state@(State clients) =
  state { clients = Map.insert identity connection clients }

removeClient :: Identity -> State -> State
removeClient identity state@(State clients) =
  state { clients = Map.delete identity clients }
