{-# LANGUAGE OverloadedStrings #-}

module Message where

import Data.Aeson    (FromJSON(parseJSON), Value(Object, String), (.:))
import Control.Monad (mzero)

import Identity      (Identity, parseIdentity)

data IncomingMessage
  = Checkin Identity
  | Message Identity

instance FromJSON IncomingMessage where
  parseJSON (Object message) = do
    name <- message .: "name"
    identityString <- message .: "identity"
    case parseIdentity identityString of
      Just identity -> case name of
        String "checkin" -> return $ Checkin identity
        _                -> return $ Message identity
      Nothing       -> fail "Unknown identity"

  parseJSON _ = mzero
