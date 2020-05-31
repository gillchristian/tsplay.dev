{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Api.Short
  ( ShortAPI,
    shortApi,
    shortServer,
  )
where

import Api.Short.Models
import Api.Short.Persistence
import Config (AppT (..), Config (..))
import qualified Control.Monad as Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.Metrics as Metrics
import Control.Monad.Reader (asks)
import Data.HashMap.Lazy (HashMap)
import Data.IORef (readIORef)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Lens.Micro ((^.))
import qualified Network.URI as URI
import Servant ((:<|>) (..), (:>))
import qualified Servant
import qualified System.Metrics.Counter as Counter
import qualified Web.Hashids as Hashids
import Prelude

type ShortAPI =
  HealthzRoute
    :<|> CreateRoute
    :<|> ListAllRoute
    :<|> StatsRoute
    :<|> MetricsRoute
    :<|> VisitRoute

type HealthzRoute =
  "healthz"
    :> Servant.Get '[Servant.PlainText] Text

type CreateRoute =
  "api" :> "short"
    :> Servant.ReqBody '[Servant.JSON] CreateBody
    :> Servant.Post '[Servant.JSON] CreateResponse

type ListAllRoute =
  "api" :> "short"
    :> Servant.Get '[Servant.JSON] [ShortenedUrl]

type StatsRoute =
  "api" :> "short" :> "stats"
    :> Servant.Get '[Servant.JSON] Stats

type MetricsRoute =
  "metrics"
    :> Servant.Get '[Servant.JSON] (HashMap Text Int64)

type VisitRoute =
  Servant.Capture "short" Text
    :> Servant.Get '[Servant.JSON] ()

shortServer :: MonadIO m => Servant.ServerT ShortAPI (AppT m)
shortServer =
  healthzHandler
    :<|> createHandler
    :<|> listAllHandler
    :<|> statsHandler
    :<|> waiMetricsHandler
    :<|> visitHandler

shortApi :: Servant.Proxy ShortAPI
shortApi = Servant.Proxy

healthzHandler :: MonadIO m => AppT m Text
healthzHandler = pure "Ok"

isValidURL :: Text -> Bool
isValidURL =
  (&&) <$> (URI.isURI . Text.unpack) <*> Text.isInfixOf "typescriptlang.org"

createHandler :: MonadIO m => CreateBody -> AppT m CreateResponse
createHandler CreateBody {..} = do
  Metrics.increment "createShort"
  Monad.unless (isValidURL createUrl) $ Servant.throwError Servant.err400
  mbShortened <- findUrlByLong createUrl
  baseUrl <- asks configBaseUrl
  case mbShortened of
    Nothing -> do
      seed <- maybe (Servant.throwError Servant.err500) pure =<< nextShortRefCounter
      short <- asks (decodeUtf8 . flip Hashids.encode seed . configHashidsCtx)
      Monad.void $ insertUrl $ ShortenedUrl short createUrl 0 (fromMaybe False createExpires)
      incLinksCreated $ fromMaybe Other createCreatedOn
      pure $ CreateResponse (baseUrl <> "/" <> short) -- TODO: return 201
    Just ShortenedUrl {..} ->
      pure $ CreateResponse (baseUrl <> "/" <> shortenedShort)

-- TODO: pagination + total count
listAllHandler :: MonadIO m => AppT m [ShortenedUrl]
listAllHandler = do
  Metrics.increment "listAll"
  findAllUrls

-- TODO: use a separate table
statsHandler :: MonadIO m => AppT m Stats
statsHandler = do
  Metrics.increment "stats"
  urlsStats

visitHandler :: MonadIO m => Text -> AppT m ()
visitHandler short = do
  Metrics.increment "visitShort"
  mbUrl <- findUrlByShortToVisit short
  case mbUrl of
    Just ShortenedUrl {..} -> do
      incLinksVisited
      redirectTo shortenedUrl
    Nothing ->
      redirectTo =<< asks configClientUrl

redirectTo :: MonadIO m => Text -> AppT m ()
redirectTo url =
  Servant.throwError $
    Servant.err302 {Servant.errHeaders = [("Location", encodeUtf8 url)]}

-- | Return wai metrics as JSON
waiMetricsHandler :: MonadIO m => AppT m (HashMap Text Int64)
waiMetricsHandler = do
  metr <- Metrics.getMetrics
  liftIO $ mapM Counter.read =<< readIORef (metr ^. Metrics.metricsCounters)
