module StateQuery where

import Identity (Identity)
import Query    (Query, Result)
import State    (State)

data ServiceError
  = IdentityAlreadyCheckedIn Identity
  | IdentityNotCheckedIn Identity
  | NotConnected
  | MessageBeforeCheckIn
  | AlreadyCheckedInAs Identity
  | CheckOutWithoutCheckIn
  | CheckOutWrongIdentity Identity Identity
  deriving (Show)

type StateQuery a b = Query State ServiceError a b
type QueryResult a  = Result State ServiceError a
