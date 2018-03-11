{-# LANGUAGE OverloadedStrings #-}

module ServiceIdentity where

import           Data.Text  (Text, pack, unpack, splitOn)
import           Data.Semigroup ((<>))
import           Data.Aeson (ToJSON, FromJSON, Value(String, Object), object, (.=), (.:))
import qualified Data.Aeson  as Aeson
import           Control.Monad (MonadPlus, mzero)
import           Data.UUID (UUID)

import           Game (Game, stringify, parseGame)

data ServiceType
  = StateService
  | FrontService
  | SandboxService Game
  | InitService
  | ContainerService
  deriving (Eq, Ord)

instance Show ServiceType where
  show StateService          = "state-service"
  show FrontService          = "front-service"
  show (SandboxService game) = "sandbox-service" <> ":" <> unpack (stringify game)
  show InitService           = "init-service"
  show ContainerService      = "container-service"

instance ToJSON ServiceType where
  toJSON = Aeson.String . pack . show

instance FromJSON ServiceType where
  parseJSON (String serviceType) = parseServiceType serviceType
  parseJSON serviceType          = fail $ "Unrecognized service type: " <> show serviceType

parseServiceType :: (MonadPlus m) => Text -> m ServiceType
parseServiceType "state-service"     = pure StateService
parseServiceType "front-service"     = pure FrontService
parseServiceType "init-service"      = pure InitService
parseServiceType "container-service" = pure ContainerService
parseServiceType string =
  case splitOn ":" string of
    ["sandbox-service", gameType]   -> SandboxService <$> parseGame gameType
    _                               -> mzero

data ServiceIdentity
  = Messenger
  | ServiceIdentity ServiceType UUID
  deriving (Eq, Ord, Show)

instance ToJSON ServiceIdentity where
  toJSON Messenger = String "messenger"
  toJSON (ServiceIdentity serviceType uuid) = object
    [ "serviceType" .= serviceType
    , "id"   .= uuid
    ]

instance FromJSON ServiceIdentity where
  parseJSON (String "mesenger") = pure Messenger
  parseJSON (Object serviceIdentity) =
    ServiceIdentity
      <$> serviceIdentity .: "type"
      <*> serviceIdentity .: "id"
  parseJSON serviceIdentity = fail $ "Bad service identity: " <> show serviceIdentity
