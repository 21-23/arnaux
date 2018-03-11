{-# LANGUAGE OverloadedStrings #-}

module Message where

import Data.Aeson    (FromJSON(parseJSON), Value(Object, String), (.:))

import ServiceIdentity (ServiceIdentity, ServiceType)

data IncomingMessage
  = CheckIn ServiceType
  | CheckOut ServiceIdentity
  | Message

instance FromJSON IncomingMessage where
  parseJSON (Object message) = do
    name <- message .: "name"
    case name of
      String "checkin"  -> CheckIn <$> message .: "serviceType"
      String "checkout" -> CheckOut <$> message .: "identity"
      String _          -> return Message
      _                 -> fail "Message name is not a string"
  parseJSON _ = fail "Bad message format"
