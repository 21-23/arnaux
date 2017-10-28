{-# LANGUAGE NamedFieldPuns #-}

module State where

import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict     as Map

import           Identity           (Identity)
import           Connection         ( Connection
                                    , ConnectionState (Accepted, CheckedIn)
                                    )

data State = State
  { connections :: Map Connection ConnectionState
  , clients     :: Map Identity Connection
  }

empty :: State
empty = State
  { connections = Map.empty
  , clients     = Map.empty
  }

connect :: Connection -> State -> State
connect connectionId state@(State connections _) =
  state { connections = Map.insert connectionId Accepted connections }

disconnect :: Connection -> State -> State
disconnect connection state@(State connections clients) =
  case Map.lookup connection connections of
    Just Accepted             -> state { connections = Map.delete connection connections }
    Just (CheckedIn identity) -> state { connections = Map.delete connection connections
                                       , clients     = Map.delete identity clients
                                       }
    Nothing                   -> state -- inconsistent state!

checkIn :: Connection -> Identity -> State -> State
checkIn connection identity state@(State connections clients) =
  state { connections = Map.adjust (const $ CheckedIn identity) connection connections
        , clients     = Map.insert identity connection clients
        }

checkOut :: Identity -> State -> State
checkOut identity state@(State _ clients) =
  state { clients = Map.delete identity clients }
