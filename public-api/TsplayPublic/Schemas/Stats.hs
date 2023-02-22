{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TsplayPublic.Schemas.Stats where

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

data Stats = Stats
    { shortened :: GHC.Int.Int64
    , visits :: GHC.Int.Int64
    }
    deriving (Show)

instance Data.Aeson.ToJSON Stats where
    toJSON Stats{..} =
        Data.Aeson.object
            ( [ "shortened" Data.Aeson..= shortened
              , "visits" Data.Aeson..= visits
              ]
            )

    toEncoding Stats{..} =
        Data.Aeson.Encoding.pairs
            ( Data.Aeson.Encoding.pair "shortened" (Data.Aeson.toEncoding shortened)
                <> Data.Aeson.Encoding.pair "visits" (Data.Aeson.toEncoding visits)
            )

instance Data.Aeson.FromJSON Stats where
    parseJSON = Data.Aeson.withObject "Stats" $ \o ->
        Stats
            <$> o Data.Aeson..: "shortened"
            <*> o Data.Aeson..: "visits"
