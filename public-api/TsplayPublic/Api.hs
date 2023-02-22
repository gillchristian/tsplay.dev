{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TsplayPublic.Api where

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

import TsplayPublic.Request
import TsplayPublic.Response

import TsplayPublic.Schemas.CreateRequest
import TsplayPublic.Schemas.Error
import TsplayPublic.Schemas.ShortenedUrl
import TsplayPublic.Schemas.ShortenedUrls
import TsplayPublic.Schemas.Stats

import TsplayPublic.Response.CreateShort
import TsplayPublic.Response.HealthCheck
import TsplayPublic.Response.ListAllShorts
import TsplayPublic.Response.Stats
import TsplayPublic.Response.VisitShortened
import TsplayPublic.Response.VisitTsplay

data Api m = Api
    { createShort ::
        CreateRequest ->
        m CreateShortResponse
    -- ^ Create a shortened URL
    , healthCheck ::
        m HealthCheckResponse
    -- ^ Health check endpoint
    , listAllShorts ::
        m ListAllShortsResponse
    -- ^ List all shortened URLs
    , stats ::
        m StatsResponse
    -- ^ Stats
    , visitShortened ::
        -- @short@
        Data.Text.Text ->
        m VisitShortenedResponse
    -- ^ Visit shortened URL
    , visitTsplay ::
        m VisitTsplayResponse
    -- ^ Visit Tsplay.dev
    }

application :: (Control.Monad.IO.Class.MonadIO m) => (forall a. Network.Wai.Request -> m a -> IO a) -> Api m -> Network.Wai.Application -> Network.Wai.Application
application run api notFound request respond =
    case Network.Wai.pathInfo request of
        [] ->
            case Network.Wai.requestMethod request of
                "GET" ->
                    run
                        request
                        ( do
                            response <- visitTsplay api
                            Control.Monad.IO.Class.liftIO (respond $! (toResponse response))
                        )
                x ->
                    unsupportedMethod x
        ["api", "short"] ->
            case Network.Wai.requestMethod request of
                "GET" ->
                    run
                        request
                        ( do
                            response <- listAllShorts api
                            Control.Monad.IO.Class.liftIO (respond $! (toResponse response))
                        )
                "POST" ->
                    parseRequestBody
                        [jsonBodyParser]
                        ( \body request respond ->
                            run
                                request
                                ( do
                                    response <- createShort api body
                                    Control.Monad.IO.Class.liftIO (respond $! (toResponse response))
                                )
                        )
                        request
                        respond
                x ->
                    unsupportedMethod x
        ["api", "short", "stats"] ->
            case Network.Wai.requestMethod request of
                "GET" ->
                    run
                        request
                        ( do
                            response <- stats api
                            Control.Monad.IO.Class.liftIO (respond $! (toResponse response))
                        )
                x ->
                    unsupportedMethod x
        ["healthz"] ->
            case Network.Wai.requestMethod request of
                "GET" ->
                    run
                        request
                        ( do
                            response <- healthCheck api
                            Control.Monad.IO.Class.liftIO (respond $! (toResponse response))
                        )
                x ->
                    unsupportedMethod x
        [__short] ->
            pathVariable
                __short
                ( \__short request respond ->
                    case Network.Wai.requestMethod request of
                        "GET" ->
                            run
                                request
                                ( do
                                    response <- visitShortened api __short
                                    Control.Monad.IO.Class.liftIO (respond $! (toResponse response))
                                )
                        x ->
                            unsupportedMethod x
                )
                request
                respond
        _ ->
            notFound request respond
  where
    unsupportedMethod _ =
        respond (Network.Wai.responseBuilder Network.HTTP.Types.status405 [] mempty)
{-# INLINEABLE application #-}
