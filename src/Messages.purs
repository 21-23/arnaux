module Messages
  ( IncomingMessage(..)
  ) where

import Prelude

import Data.Argonaut
  ( class DecodeJson
  , decodeJson
  , jsonParser
  , (.?)
  )

data IncomingMessage
  = CheckIn { identity :: String }
  | Message { to :: String }

instance decodeJson :: DecodeJson IncomingMessage where
decodeJson json = do
  message <- decodeJson json
  messageType <- message .? "type"
  case messageType of
    "checkin" -> do
      payloadString <- message .? "payload"
      payloadJson <- jsonParser payloadString
      payload <- decodeJson payloadJson
      identity <- payload .? "identity"
      pure $ CheckIn {identity}
    _ -> do
      to <- message .? "destination"
      pure $ Message {to}
