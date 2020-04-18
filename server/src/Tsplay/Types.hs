{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Tsplay.Types
  ( App,
    CreateBody (..),
    CreateResponse (..),
    DbConfig (..),
    Environment (..),
    AppConfig (..),
    AppEnv (..),
    ShortenedUrl (..),
  )
where

import Control.Monad.Trans.Reader (ReaderT)
import qualified Data.Aeson as Json
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import GHC.Generics
import qualified Servant
import Text.Casing (camel)
import qualified Web.Hashids as Hashids
import Prelude

-- Application Monad

type App = ReaderT AppEnv Servant.Handler

-- -- Environment ---

data DbConfig
  = DbConfig
      { dbHostname :: String,
        dbName :: Text,
        dbUser :: Text,
        dbPassword :: Text
      }
  deriving stock (Generic, Show)

instance Json.FromJSON DbConfig where
  parseJSON = Json.genericParseJSON $ dropLabelPrefix "db"

data Environment
  = Production
  | Development
  deriving stock (Generic, Show)

instance Json.FromJSON Environment where
  parseJSON = Json.genericParseJSON camelTags

instance Json.ToJSON Environment where
  toJSON = Json.genericToJSON camelTags

data AppConfig
  = AppConfig
      { appDatabase :: DbConfig,
        appBaseUrl :: String,
        appClientUrl :: String,
        appEnv :: Environment
      }
  deriving stock (Generic, Show)

instance Json.FromJSON AppConfig where
  parseJSON = Json.genericParseJSON $ dropLabelPrefix "app"

data AppEnv
  = AppEnv
      { conf :: AppConfig,
        hashidsCtx :: Hashids.HashidsContext
      }

-- -- Models ---

data CreateBody
  = CreateBody
      { createUrl :: String,
        createShort :: Maybe String
      }
  deriving stock (Generic, Show)

instance Json.ToJSON CreateBody where
  toJSON = Json.genericToJSON $ dropLabelPrefix "create"

instance Json.FromJSON CreateBody where
  parseJSON = Json.genericParseJSON $ dropLabelPrefix "create"

newtype CreateResponse
  = CreateResponse
      { createShortened :: String
      }
  deriving stock (Generic, Show)

instance Json.ToJSON CreateResponse where
  toJSON = Json.genericToJSON $ dropLabelPrefix "create"

instance Json.FromJSON CreateResponse where
  parseJSON = Json.genericParseJSON $ dropLabelPrefix "create"

data ShortenedUrl
  = ShortenedUrl
      { shortenedId :: String,
        shortenedShort :: String,
        shortenedUrl :: String,
        shortenedVisits :: Int
      }
  deriving stock (Generic, Show)

instance Json.ToJSON ShortenedUrl where
  toJSON = Json.genericToJSON $ dropLabelPrefix "shortened"

instance Json.FromJSON ShortenedUrl where
  parseJSON = Json.genericParseJSON $ dropLabelPrefix "shortened"

-- -- Utils ---

dropLabelPrefix :: String -> Json.Options
dropLabelPrefix prefix =
  Json.defaultOptions {Json.fieldLabelModifier = camel . stripPrefix prefix}

camelTags :: Json.Options
camelTags =
  Json.defaultOptions {Json.constructorTagModifier = camel}

stripPrefix :: Eq a => [a] -> [a] -> [a]
stripPrefix prefix str = Maybe.fromMaybe str $ List.stripPrefix prefix str
