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
  = CheckIn { name :: String }
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
      name <- payload .? "name"
      pure $ CheckIn {name}
    _ -> do
      to <- message .? "destination"
      pure $ Message {to}
