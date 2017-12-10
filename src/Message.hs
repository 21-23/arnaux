{-# LANGUAGE OverloadedStrings #-}

module Message where

import Data.Aeson    (FromJSON(parseJSON),
                      Value(Object, String),
                      (.:),
                      ToJSON(toJSON),
                      object,
                      (.=)
                      )
import Control.Monad (mzero)
import Data.Monoid   ((<>))
import Data.Text     (Text)

import Identity      (Identity, parseIdentity)

data IncomingMessage
  = CheckIn Identity
  | CheckOut Identity
  | Message

instance FromJSON IncomingMessage where
  parseJSON (Object message) = do
    name <- message .: "name"
    case name of
      String "checkin" -> do
        identityString <- message .: "identity"
        case parseIdentity identityString of
          Just identity -> return $ CheckIn identity
          Nothing       -> fail   $ "Unrecognized identity " <> identityString
      String "checkout" -> do
        identityString <- message .: "identity"
        case parseIdentity identityString of
          Just identity -> return $ CheckOut identity
          Nothing       -> fail   $ "Unrecognized identity " <> identityString
      String _         -> return Message
      _                -> fail   "Message name is not a string"
  parseJSON _ = mzero

newtype OutgoingMessage = Start Identity

instance ToJSON OutgoingMessage where
  toJSON (Start identity) = object
    [ "name"     .= ("start" :: Text)
    , "identity" .= identity
    ]
