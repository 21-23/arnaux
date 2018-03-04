{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}

module Envelope where

import Control.Monad (mzero)
import Data.Aeson    (Value(Object), FromJSON(parseJSON), (.:), ToJSON(toJSON), object, (.=))
import Identity      (Identity, parseIdentity)

data Envelope a = Envelope
  { to :: Identity
  , message :: a
  }

instance (FromJSON msg) => FromJSON (Envelope msg) where
  parseJSON (Object envelope) = do
    to <- envelope .: "to"
    case parseIdentity to of
      Just identity -> Envelope <$> pure identity <*> envelope .: "message"
      Nothing       -> fail "Unknown identity"
  parseJSON _ = mzero

instance (ToJSON msg) => ToJSON (Envelope msg) where
  toJSON (Envelope to message) = object
    [ "to"      .= to
    , "message" .= message
    ]
