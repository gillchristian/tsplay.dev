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

module Tsplay.Api
  ( tsplayApi,
  )
where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Reader as ReaderT
import qualified Data.ByteString.Char8 as B
import qualified Data.List as List
import Data.Text (Text)
import qualified Network.URI as URI
import Servant ((:<|>) (..), (:>))
import qualified Servant
import Tsplay.Persistence
import Tsplay.Types
import qualified Web.Hashids as Hashids
import Prelude

type TsplayAPI =
  HealthzRoute
    :<|> CreateRoute
    :<|> ListAllRoute
    :<|> StatsRoute
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

type VisitRoute =
  Servant.Capture "short" String
    :> Servant.Get '[Servant.JSON] ()

apiRoutes :: Servant.ServerT TsplayAPI App
apiRoutes =
  healthzHandler
    :<|> createHandler
    :<|> listAllHandler
    :<|> statsHandler
    :<|> visitHandler

tsplayApi :: Servant.Context '[AppEnv] -> AppEnv -> Servant.Application
tsplayApi ctx env =
  Servant.serveWithContext apiProxy ctx $
    Servant.hoistServerWithContext apiProxy ctxProxy runWithEnv apiRoutes
  where
    runWithEnv = (`ReaderT.runReaderT` env)
    apiProxy = Servant.Proxy :: Servant.Proxy TsplayAPI
    ctxProxy = Servant.Proxy :: Servant.Proxy '[AppEnv]

healthzHandler :: App Text
healthzHandler = pure "Ok"

isValidURL :: String -> Bool
isValidURL = (&&) <$> URI.isURI <*> List.isInfixOf "typescriptlang.org"

createHandler :: CreateBody -> App CreateResponse
createHandler CreateBody {..} = do
  -- TODO: use provided 'short'
  Monad.unless (isValidURL createUrl) $ Servant.throwError Servant.err400
  mbShortened <- findUrlByLong createUrl
  baseUrl <- appBaseUrl . conf <$> ReaderT.ask
  case mbShortened of
    Nothing -> do
      seed <- maybe (Servant.throwError Servant.err500) pure =<< nextShortRefCounter
      short <- B.unpack . flip Hashids.encode seed . hashidsCtx <$> ReaderT.ask
      Monad.void $ insertUrl $ ShortenedUrl "" short createUrl 0
      pure $ CreateResponse (baseUrl ++ "/" ++ short) -- TODO: return 201
    Just ShortenedUrl {..} ->
      pure $ CreateResponse (baseUrl ++ "/" ++ shortenedShort)

-- TODO: pagination + total count
listAllHandler :: App [ShortenedUrl]
listAllHandler = findAllUrls

statsHandler :: App Stats
statsHandler = urlsStats

visitHandler :: String -> App ()
visitHandler short = do
  mbUrl <- findUrlByShortToVisit short
  case mbUrl of
    Just ShortenedUrl {..} ->
      redirectTo shortenedUrl
    Nothing ->
      redirectTo =<< appClientUrl . conf <$> ReaderT.ask

redirectTo :: String -> App ()
redirectTo url =
  Servant.throwError $
    Servant.err302 {Servant.errHeaders = [("Location", B.pack url)]}
