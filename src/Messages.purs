module Messages
  ( IncomingMessage(..)
  ) where

import Prelude

import Data.Argonaut
  ( class DecodeJson
  , decodeJson
  , (.?)
  )

data IncomingMessage
  = CheckIn { identity :: String }
  | Message { to :: String }

instance decodeJson :: DecodeJson IncomingMessage where
decodeJson json = do
  message <- decodeJson json
  payload <- message .? "message"
  messageType <- payload .? "name"
  case messageType of
    "checkin" -> do
      identity <- payload .? "identity"
      pure $ CheckIn {identity}
    _ -> do
      to <- message .? "to"
      pure $ Message {to}
