{-# LANGUAGE OverloadedStrings #-}

module Effect where

import           Data.ByteString.Lazy (ByteString)
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import qualified Data.Text.IO         as TextIO
import qualified Network.WebSockets   as WebSocket

import           Connection           (Connection (Connection))

data Effect
  = Log Text
  | Send Connection ByteString

handle :: Effect -> IO ()
handle (Log string) = TextIO.putStrLn $ "🕊  " <> string
handle (Send (Connection _ connection) string) =
  WebSocket.sendTextData connection string
