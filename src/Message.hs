{-# LANGUAGE OverloadedStrings #-}

module Message where

import Data.Aeson    (FromJSON(parseJSON), Value(Object, String), (.:))
import Control.Monad (mzero)
import Data.Monoid   ((<>))

import Identity      (Identity, parseIdentity)

data IncomingMessage
  = Checkin Identity
  | Message

instance FromJSON IncomingMessage where
  parseJSON (Object message) = do
    name <- message .: "name"
    case name of
      String "checkin" -> do
        identityString <- message .: "identity"
        case parseIdentity identityString of
          Just identity -> return $ Checkin identity
          Nothing       -> fail   $ "Unrecognized identity " <> identityString
      String _         -> return Message
      _                -> fail   "Message name is not a string"
  parseJSON _ = mzero
