{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Tsplay.Server
  ( runServer,
  )
where

import qualified Data.Aeson as Json
import qualified Data.ByteString.Char8 as B
import Data.Default.Class (Default (def))
import Network.HTTP.Types.Status (status302)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors)
import qualified Network.Wai.Middleware.RequestLogger as Log
import Network.Wai.Middleware.RequestLogger.JSON (formatAsJSON)
import Servant (Context ((:.), EmptyContext))
import qualified System.Environment as Sys
import qualified System.Exit as Exit
import Tsplay.Api
import Tsplay.Types
import qualified Web.Hashids as Hashids
import Prelude

runServer :: IO ()
runServer = do
  (env, port) <- loadAppEnv
  logger <- Log.mkRequestLogger loggerSettings
  let client = B.pack $ appClientUrl $ conf env
  startMsg port
  Warp.run port
    $ appMiddleware logger client
    $ tsplayApi (env :. EmptyContext) env

loggerSettings :: Log.RequestLoggerSettings
loggerSettings =
  def {Log.outputFormat = Log.CustomOutputFormatWithDetails formatAsJSON}

startMsg :: Int -> IO ()
startMsg port =
  putStrLn $ "{ \"init\": \"Server starting in port " ++ show port ++ "\" }"

loadAppEnv :: IO (AppEnv, Int)
loadAppEnv = do
  mbConf <- Json.decodeFileStrict ".env.json"
  env <- case mbConf of
    Just c -> pure $ AppEnv c $ Hashids.hashidsMinimum salt 6
    Nothing -> Exit.die "Invalid configuration"
  port <- maybe 8080 read <$> Sys.lookupEnv "PORT"
  pure (env, port)

-- This is no secret, only used to generate short URLs
salt :: B.ByteString
salt = "5F,k:y]C6h<cZwN>LiEt6?^M4AEqE]|@anRG=oF<nL6M8HP2UT?Hp PZm>C4=+4>"

appMiddleware :: Wai.Middleware -> B.ByteString -> Wai.Middleware
appMiddleware logger client = corsified . logger . rootRedirectTo client

rootRedirectTo :: B.ByteString -> Wai.Middleware
rootRedirectTo url =
  Wai.ifRequest isRoot $ Wai.modifyResponse (addRedirectHeaders url . set302)

set302 :: Wai.Response -> Wai.Response
set302 = Wai.mapResponseStatus $ const status302

isRoot :: Wai.Request -> Bool
isRoot = null . Wai.pathInfo

addRedirectHeaders :: B.ByteString -> Wai.Response -> Wai.Response
addRedirectHeaders url = Wai.mapResponseHeaders $ (:) ("Location", url)

corsified :: Wai.Middleware
corsified = cors (const $ Just corsResourcePolicy)

-- TODO: allow only from client's url
corsResourcePolicy :: CorsResourcePolicy
corsResourcePolicy = CorsResourcePolicy
  { corsOrigins = Nothing,
    corsMethods = ["OPTIONS", "GET", "PUT", "POST"],
    corsRequestHeaders = ["Authorization", "Content-Type"],
    corsExposedHeaders = Nothing,
    corsMaxAge = Nothing,
    corsVaryOrigin = False,
    corsRequireOrigin = False,
    corsIgnoreFailures = False
  }
