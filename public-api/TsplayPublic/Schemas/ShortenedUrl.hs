{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TsplayPublic.Schemas.ShortenedUrl where

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

data ShortenedUrl = ShortenedUrl
    { expires :: GHC.Types.Bool
    , short :: Data.Text.Text
    , shortened :: Data.Text.Text
    , url :: Data.Text.Text
    , visits :: GHC.Int.Int64
    }
    deriving (Show)

instance Data.Aeson.ToJSON ShortenedUrl where
    toJSON ShortenedUrl{..} =
        Data.Aeson.object
            ( [ "expires" Data.Aeson..= expires
              , "short" Data.Aeson..= short
              , "shortened" Data.Aeson..= shortened
              , "url" Data.Aeson..= url
              , "visits" Data.Aeson..= visits
              ]
            )

    toEncoding ShortenedUrl{..} =
        Data.Aeson.Encoding.pairs
            ( Data.Aeson.Encoding.pair "expires" (Data.Aeson.toEncoding expires)
                <> Data.Aeson.Encoding.pair "short" (Data.Aeson.toEncoding short)
                <> Data.Aeson.Encoding.pair "shortened" (Data.Aeson.toEncoding shortened)
                <> Data.Aeson.Encoding.pair "url" (Data.Aeson.toEncoding url)
                <> Data.Aeson.Encoding.pair "visits" (Data.Aeson.toEncoding visits)
            )

instance Data.Aeson.FromJSON ShortenedUrl where
    parseJSON = Data.Aeson.withObject "ShortenedUrl" $ \o ->
        ShortenedUrl
            <$> o Data.Aeson..: "expires"
            <*> o Data.Aeson..: "short"
            <*> o Data.Aeson..: "shortened"
            <*> o Data.Aeson..: "url"
            <*> o Data.Aeson..: "visits"
