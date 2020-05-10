{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.Short.Models
  ( CreateBody (..),
    CreateResponse (..),
    ShortenedUrl (..),
    Stats (..),
  )
where

import qualified Data.Aeson as Json
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import Database.PostgreSQL.Simple (FromRow)
import GHC.Generics
import Text.Casing (camel)
import Prelude

-- -- Models ---

data CreateBody
  = CreateBody
      { createUrl :: Text,
        createShort :: Maybe Text
      }
  deriving stock (Generic, Show)

instance Json.ToJSON CreateBody where
  toJSON = Json.genericToJSON $ dropLabelPrefix "create"

instance Json.FromJSON CreateBody where
  parseJSON = Json.genericParseJSON $ dropLabelPrefix "create"

newtype CreateResponse
  = CreateResponse
      { createShortened :: Text
      }
  deriving stock (Generic, Show)

instance Json.ToJSON CreateResponse where
  toJSON = Json.genericToJSON $ dropLabelPrefix "create"

instance Json.FromJSON CreateResponse where
  parseJSON = Json.genericParseJSON $ dropLabelPrefix "create"

data ShortenedUrl
  = ShortenedUrl
      { shortenedShort :: Text,
        shortenedUrl :: Text,
        shortenedVisits :: Int
      }
  deriving stock (Generic, Show)
  deriving (FromRow)

instance Json.ToJSON ShortenedUrl where
  toJSON = Json.genericToJSON $ dropLabelPrefix "shortened"

instance Json.FromJSON ShortenedUrl where
  parseJSON = Json.genericParseJSON $ dropLabelPrefix "shortened"

data Stats
  = Stats
      { statsTotalShortened :: Int,
        statsTotalVisits :: Int
      }
  deriving stock (Generic, Show)
  deriving (FromRow)

instance Json.ToJSON Stats where
  toJSON = Json.genericToJSON $ dropLabelPrefix "stats"

instance Json.FromJSON Stats where
  parseJSON = Json.genericParseJSON $ dropLabelPrefix "stats"

-- -- Utils ---

dropLabelPrefix :: String -> Json.Options
dropLabelPrefix prefix =
  Json.defaultOptions {Json.fieldLabelModifier = camel . stripPrefix prefix}

-- camelTags :: Json.Options
-- camelTags =
--   Json.defaultOptions {Json.constructorTagModifier = camel}

stripPrefix :: Eq a => [a] -> [a] -> [a]
stripPrefix prefix str = Maybe.fromMaybe str $ List.stripPrefix prefix str
