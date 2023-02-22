{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TsplayPublic.Schemas.CreateRequest where

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

import TsplayPublic.Schemas.CreatedOn

data CreateRequest = CreateRequest
  { createdOn :: (Data.Maybe.Maybe (CreatedOn))
  , expires :: (Data.Maybe.Maybe (GHC.Types.Bool))
  , short :: (Data.Maybe.Maybe (Data.Text.Text))
  , url :: Data.Text.Text
  }
  deriving (Show)

instance Data.Aeson.ToJSON CreateRequest where
  toJSON CreateRequest{..} =
    Data.Aeson.object
      ( [ "url" Data.Aeson..= url
        ]
          ++ ["created_on" Data.Aeson..= createdOn | Just createdOn <- [createdOn]]
          ++ ["expires" Data.Aeson..= expires | Just expires <- [expires]]
          ++ ["short" Data.Aeson..= short | Just short <- [short]]
      )

  toEncoding CreateRequest{..} =
    Data.Aeson.Encoding.pairs
      ( maybe mempty (Data.Aeson.Encoding.pair "created_on" . Data.Aeson.toEncoding) createdOn
          <> maybe mempty (Data.Aeson.Encoding.pair "expires" . Data.Aeson.toEncoding) expires
          <> maybe mempty (Data.Aeson.Encoding.pair "short" . Data.Aeson.toEncoding) short
          <> Data.Aeson.Encoding.pair "url" (Data.Aeson.toEncoding url)
      )

instance Data.Aeson.FromJSON CreateRequest where
  parseJSON = Data.Aeson.withObject "CreateRequest" $ \o ->
    CreateRequest
      <$> o Data.Aeson..:? "created_on"
      <*> o Data.Aeson..:? "expires"
      <*> o Data.Aeson..:? "short"
      <*> o Data.Aeson..: "url"
