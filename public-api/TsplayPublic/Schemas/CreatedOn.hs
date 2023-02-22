{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TsplayPublic.Schemas.CreatedOn where

import qualified Control.Applicative
import qualified Control.Exception
import qualified Control.Monad
import qualified Control.Monad.IO.Class
import qualified Data.Aeson
import qualified Data.Aeson.Encoding
import qualified Data.Aeson.Parser
import qualified Data.Aeson.Types
import qualified Data.Attoparsec.ByteString
import qualified Data.List
import qualified Data.List.NonEmpty
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Time
import qualified GHC.Float
import qualified GHC.Int
import qualified GHC.Records
import qualified GHC.Types
import qualified Network.HTTP.Types
import qualified Network.Wai
import qualified Web.HttpApiData

data CreatedOn
    = CreatedOnClient
    | CreatedOnPlugin
    | CreatedOnApi
    | CreatedOnOther
    deriving (Eq, Show)

instance Data.Aeson.ToJSON CreatedOn where
    toJSON x = case x of
        CreatedOnClient -> "client"
        CreatedOnPlugin -> "plugin"
        CreatedOnApi -> "api"
        CreatedOnOther -> "other"

    toEncoding x = case x of
        CreatedOnClient -> Data.Aeson.Encoding.text "client"
        CreatedOnPlugin -> Data.Aeson.Encoding.text "plugin"
        CreatedOnApi -> Data.Aeson.Encoding.text "api"
        CreatedOnOther -> Data.Aeson.Encoding.text "other"

instance Data.Aeson.FromJSON CreatedOn where
    parseJSON = Data.Aeson.withText "CreatedOn" $ \s ->
        case s of
            "client" -> pure CreatedOnClient
            "plugin" -> pure CreatedOnPlugin
            "api" -> pure CreatedOnApi
            "other" -> pure CreatedOnOther
            _ -> fail "invalid enum value"

instance Web.HttpApiData.ToHttpApiData CreatedOn where
    toQueryParam x = case x of
        CreatedOnClient -> "client"
        CreatedOnPlugin -> "plugin"
        CreatedOnApi -> "api"
        CreatedOnOther -> "other"

instance Web.HttpApiData.FromHttpApiData CreatedOn where
    parseUrlPiece x =
        case x of
            "client" -> pure CreatedOnClient
            "plugin" -> pure CreatedOnPlugin
            "api" -> pure CreatedOnApi
            "other" -> pure CreatedOnOther
            _ -> Left "invalid enum value"
