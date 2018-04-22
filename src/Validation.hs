{-# LANGUAGE NamedFieldPuns #-}

module Validation where

import qualified Control.Monad.Trans.State.Strict as StateT
import qualified Data.Map.Strict                  as Map

import           Connection                       (Connection, ConnectionState (Accepted, CheckedIn))
import           ServiceIdentity                  (ServiceIdentity, ServiceSelector)
import           Query                            (failure, success)
import           State                            (State (clients, connections), select)
import           StateQuery                       (ServiceError (AlreadyCheckedInAs,
                                                                 CheckOutWithoutCheckIn,
                                                                 IdentityNotCheckedIn,
                                                                 MessageBeforeCheckIn,
                                                                 NotConnected,
                                                                 ServiceSelectorEmptyMatch
                                                                 ),
                                                   StateQuery)

connected :: StateQuery Connection ConnectionState
connected connection = do
  state <- StateT.get
  case Map.lookup connection $ connections state of
    Nothing              -> failure NotConnected
    Just connectionState -> success connectionState

notCheckedInYet :: StateQuery ConnectionState ()
notCheckedInYet Accepted             = success ()
notCheckedInYet (CheckedIn identity) = failure $ AlreadyCheckedInAs identity

connectionCanCheckOut :: StateQuery ConnectionState ServiceIdentity
connectionCanCheckOut Accepted             = failure CheckOutWithoutCheckIn
connectionCanCheckOut (CheckedIn identity) = success identity

connectionCanMessage :: StateQuery ConnectionState ()
connectionCanMessage Accepted      = failure MessageBeforeCheckIn
connectionCanMessage (CheckedIn _) = success ()

identityIsCheckedIn :: StateQuery ServiceIdentity Connection
identityIsCheckedIn identity = do
  clients <- StateT.gets State.clients
  case Map.lookup identity clients of
    Nothing         -> failure $ IdentityNotCheckedIn identity
    Just connection -> success connection

selectServiceConnection :: StateQuery ServiceSelector Connection
selectServiceConnection selector = do
  selectorResult <- StateT.gets (State.select selector)
  case selectorResult of
    Nothing              -> failure $ ServiceSelectorEmptyMatch selector
    Just (_, connection) -> success connection
