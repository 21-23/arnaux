module StateQuery where

import ServiceIdentity (ServiceIdentity, ServiceSelector)
import Query    (Query)
import State    (State)

data ServiceError
  = IdentityNotCheckedIn ServiceIdentity
  | NotConnected
  | MessageBeforeCheckIn
  | AlreadyCheckedInAs ServiceIdentity
  | CheckOutWithoutCheckIn
  | CheckOutWrongIdentity ServiceIdentity ServiceIdentity
  | ServiceSelectorEmptyMatch ServiceSelector
  deriving (Show)

type StateQuery a b = Query State ServiceError a b
