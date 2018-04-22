{-# LANGUAGE NamedFieldPuns #-}

module State where

import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict     as Map
import           System.Random      (StdGen, random)
import           Data.List          (find)

import           ServiceIdentity    (ServiceIdentity(ServiceIdentity),
                                     ServiceType,
                                     ServiceSelector(Messenger, AnyOfType, Service))
import           Connection         ( Connection
                                    , ConnectionState (Accepted, CheckedIn)
                                    )

data State = State
  { connections  :: Map Connection ConnectionState
  , clients      :: Map ServiceIdentity Connection
  , stdRandomGen :: StdGen
  }

empty :: StdGen -> State
empty stdRandomGen = State
  { connections = Map.empty
  , clients     = Map.empty
  , stdRandomGen
  }

connect :: Connection -> State -> State
connect connectionId state@State{connections} =
  state { connections = Map.insert connectionId Accepted connections }

disconnect :: Connection -> State -> State
disconnect connection state@State{connections, clients} =
  case Map.lookup connection connections of
    Just Accepted             -> state { connections = Map.delete connection connections }
    Just (CheckedIn identity) -> state { connections = Map.delete connection connections
                                       , clients     = Map.delete identity clients
                                       }
    Nothing                   -> state -- inconsistent state!

checkIn :: Connection -> ServiceType -> State -> (ServiceIdentity, State)
checkIn connection serviceType state@State{connections, clients, stdRandomGen} =
  let (uuid, newRandomGen) = random stdRandomGen
      serviceIdentity = ServiceIdentity serviceType uuid
      newState = state { connections  = Map.adjust (const $ CheckedIn serviceIdentity) connection connections
                       , clients      = Map.insert serviceIdentity connection clients
                       , stdRandomGen = newRandomGen
                       }
   in (serviceIdentity, newState)

checkOut :: ServiceIdentity -> State -> State
checkOut identity state@State{connections, clients} =
  case Map.lookup identity clients of
    Just connection ->
      state { connections = Map.adjust (const Accepted) connection connections
            , clients     = Map.delete identity clients
            }
    Nothing         -> state -- inconsistent state!

select :: ServiceSelector -> State -> Maybe (ServiceIdentity, Connection)
select selector State{clients} =
  find (match selector . fst) (Map.toList clients)
    where
      match Messenger _  = False -- messenger is not included in the service list
      match (AnyOfType selectorServiceType) (ServiceIdentity serviceType _)
        = selectorServiceType == serviceType
      match (Service selectorServiceIdentity) serviceIdentity
        = selectorServiceIdentity == serviceIdentity
