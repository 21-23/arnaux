{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}

module Envelope where

import Control.Monad (mzero)
import Data.Aeson    (Value(Object), FromJSON(parseJSON), (.:))
import Message       (IncomingMessage)
import Identity      (Identity, parseIdentity)

data Envelope a = Envelope
  { to :: Identity
  , message :: a
  }

instance FromJSON (Envelope IncomingMessage) where
  parseJSON (Object envelope) = do
    to <- envelope .: "to"
    case parseIdentity to of
      Just identity -> Envelope <$> pure identity <*> envelope .: "message"
      Nothing       -> fail "Unknown identity"
  parseJSON _ = mzero
