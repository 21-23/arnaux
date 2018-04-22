module Connection where

import           Data.UUID          (UUID)
import qualified Network.WebSockets as WebSockets

import           ServiceIdentity    (ServiceIdentity)

data Connection = Connection UUID WebSockets.Connection

instance Eq Connection where
  (==) (Connection idA _) (Connection idB _) = idA == idB

instance Ord Connection where
  compare (Connection idA _) (Connection idB _) = compare idA idB

instance Show Connection where
  show (Connection connectionId _) = show connectionId

data ConnectionState
  = Accepted
  | CheckedIn ServiceIdentity
