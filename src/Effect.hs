{-# LANGUAGE OverloadedStrings #-}

module Effect where

import           Data.ByteString.Lazy (ByteString)
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import qualified Network.WebSockets   as WebSocket
import           System.Logger        (Logger, Level)
import qualified System.Logger        as Logger
import           Data.Foldable        (traverse_)

import           Connection           (Connection (Connection))

data Effect
  = Log Level Text
  | Send Connection ByteString
  | List [Effect]

handle :: Logger -> Effect -> IO ()
handle logger (Log level string) =
  Logger.log logger level $ Logger.msg $ "🕊  " <> string
handle _      (Send (Connection _ connection) string) =
  WebSocket.sendTextData connection string
handle logger (List effects) =
  traverse_ (handle logger) effects
