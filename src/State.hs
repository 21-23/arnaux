{-# LANGUAGE NamedFieldPuns #-}

module State where

import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Sequence        (Seq, (|>))
import qualified Data.Sequence        as Seq
import           Data.ByteString.Lazy (ByteString)

import           Identity           (Identity)
import           Connection         ( Connection
                                    , ConnectionState (Accepted, CheckedIn)
                                    )

data State = State
  { connections :: Map Connection ConnectionState
  , clients     :: Map Identity Connection
  , queues      :: Map Identity (Seq ByteString)
  }

empty :: State
empty = State
  { connections = Map.empty
  , clients     = Map.empty
  , queues      = Map.empty
  }

connect :: Connection -> State -> State
connect connectionId state@(State connections _ _) =
  state { connections = Map.insert connectionId Accepted connections }

disconnect :: Connection -> State -> State
disconnect connection state@(State connections clients _) =
  case Map.lookup connection connections of
    Just Accepted             -> state { connections = Map.delete connection connections }
    Just (CheckedIn identity) -> state { connections = Map.delete connection connections
                                       , clients     = Map.delete identity clients
                                       }
    Nothing                   -> state -- inconsistent state!

checkIn :: Connection -> Identity -> State -> State
checkIn connection identity state@(State connections clients _) =
  state { connections = Map.adjust (const $ CheckedIn identity) connection connections
        , clients     = Map.insert identity connection clients
        }

checkOut :: Identity -> State -> State
checkOut identity state@(State connections clients _) =
  case Map.lookup identity clients of
    Just connection ->
      state { connections = Map.adjust (const Accepted) connection connections
            , clients     = Map.delete identity clients
            }
    Nothing         -> state -- inconsistent state!

enqueueMessage :: Identity -> ByteString -> State -> State
enqueueMessage recepient message state@(State _ _ queues) =
  state { queues = Map.alter update recepient queues }
    where
      update (Just queue) = Just $ queue |> message
      update Nothing      = Just $ Seq.singleton message
