{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TsplayPublic.Response.VisitShortened where

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

import TsplayPublic.Response

data VisitShortenedResponse
    = VisitShortenedResponse302 Data.Text.Text
    deriving (Show)

instance ToResponse VisitShortenedResponse where
    toResponse (VisitShortenedResponse302 __Location) =
        Network.Wai.responseBuilder Network.HTTP.Types.status302 ([("Location", Web.HttpApiData.toHeader __Location)]) mempty

instance GHC.Records.HasField "status" VisitShortenedResponse Network.HTTP.Types.Status where
    getField (VisitShortenedResponse302{}) = Network.HTTP.Types.status302
