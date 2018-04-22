{-# LANGUAGE OverloadedStrings #-}

module Message where

import Data.Aeson    (FromJSON(parseJSON),
                      ToJSON(toJSON),
                      Value(Object, String), (.:), (.=), object)

import ServiceIdentity (ServiceIdentity, ServiceType)

data IncomingMessage
  = CheckIn ServiceType
  | CheckOut ServiceIdentity
  | Message

instance FromJSON IncomingMessage where
  parseJSON (Object message) = do
    name <- message .: "name"
    case name of
      String "checkin"  -> CheckIn <$> message .: "identity"
      String "checkout" -> CheckOut <$> message .: "identity"
      String _          -> return Message
      _                 -> fail "Message name is not a string"
  parseJSON _ = fail "Bad message format"

newtype OutgoingMessage
  = CheckedIn ServiceIdentity

instance ToJSON OutgoingMessage where
  toJSON (CheckedIn serviceIdentity) = object
    [ "name"     .= String "checkedIn"
    , "identity" .= serviceIdentity
    ]
