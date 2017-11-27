{-# LANGUAGE OverloadedStrings #-}

module Effect where

import           Data.ByteString.Lazy (ByteString)
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import qualified Network.WebSockets   as WebSocket
import           System.Logger        (Logger, Level)
import qualified System.Logger        as Logger

import           Connection           (Connection (Connection))

data Effect
  = Log Level Text
  | Send Connection ByteString

handle :: Logger -> Effect -> IO ()
handle logger (Log level string) =
  Logger.log logger level $ Logger.msg $ "🕊  " <> string
handle _      (Send (Connection _ connection) string) =
  WebSocket.sendTextData connection string
