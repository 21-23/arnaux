{-# LANGUAGE NamedFieldPuns #-}

module Validation where

import qualified Control.Monad.Trans.State.Strict as StateT
import qualified Data.Map.Strict                  as Map

import           Connection                       (Connection, ConnectionState (Accepted, CheckedIn))
import           Identity                         (Identity (MetaService))
import           Query                            (failure, success)
import           State                            (State (clients, connections))
import           StateQuery                       (ServiceError (AlreadyCheckedInAs,
                                                                 CheckOutWithoutCheckIn,
                                                                 IdentityAlreadyCheckedIn,
                                                                 IdentityNotCheckedIn,
                                                                 MessageBeforeCheckIn,
                                                                 NotConnected
                                                                 ),
                                                   StateQuery,
                                                   QueryResult)

connected :: StateQuery Connection ConnectionState
connected connection = do
  state <- StateT.get
  case Map.lookup connection $ connections state of
    Nothing              -> failure NotConnected
    Just connectionState -> success connectionState

notCheckedInYet :: StateQuery ConnectionState ()
notCheckedInYet Accepted             = success ()
notCheckedInYet (CheckedIn identity) = failure $ AlreadyCheckedInAs identity

connectionCanCheckOut :: StateQuery ConnectionState Identity
connectionCanCheckOut Accepted             = failure CheckOutWithoutCheckIn
connectionCanCheckOut (CheckedIn identity) = success identity

connectionCanMessage :: StateQuery ConnectionState ()
connectionCanMessage Accepted      = failure MessageBeforeCheckIn
connectionCanMessage (CheckedIn _) = success ()

identityIsCheckedIn :: StateQuery Identity Connection
identityIsCheckedIn identity = do
  clients <- StateT.gets State.clients
  case Map.lookup identity clients of
    Nothing         -> failure $ IdentityNotCheckedIn identity
    Just connection -> success connection

identityIsAvailable :: StateQuery Identity ()
identityIsAvailable identity = do
  clients <- StateT.gets State.clients
  case Map.lookup identity clients of
    Nothing -> success ()
    Just _  -> failure $ IdentityAlreadyCheckedIn identity

metaServiceIsAvailable :: QueryResult Connection
metaServiceIsAvailable = identityIsCheckedIn MetaService
