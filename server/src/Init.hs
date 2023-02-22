{-# LANGUAGE OverloadedStrings #-}

module Init (
    runApp,
)
where

import Api (app)
import Config (
    Config (..),
    Environment (..),
    makePool,
 )
import Control.Exception (bracket)
import Data.ByteString.Char8 qualified as BS
import Data.Pool qualified as Pool
import Database (initializeDatabase)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors qualified as Cors
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Safe (readMay)
import System.Environment (lookupEnv)
import Web.Hashids qualified as Hashids
import Prelude

runApp :: IO ()
runApp = bracket acquireConfig shutdownApp runApp'
  where
    runApp' config = run (configPort config) =<< initialize config

initialize :: Config -> IO Wai.Application
initialize cfg = do
    Pool.withResource (configPool cfg) $ initializeDatabase "./database/migrations"
    pure . logger (configEnv cfg) . corsMiddleware . app $ cfg

logger :: Environment -> Wai.Middleware
logger Test = id
logger Development = logStdoutDev
logger Production = logStdout

-- | Allocates resources for 'Config'
acquireConfig :: IO Config
acquireConfig = do
    port <- lookupSetting "PORT" 9000
    env <- lookupSetting "ENV" Development
    baseUrl <- lookupSetting "BASE_URL" "http://localhost:9000"
    clientUrl <- lookupSetting "CLIENT_URL" "http://localhost:3000"
    pool <- makePool env
    pure
        Config
            { configPool = pool
            , configEnv = env
            , configPort = port
            , configHashidsCtx = Hashids.hashidsMinimum salt 6
            , configBaseUrl = baseUrl
            , configClientUrl = clientUrl
            }

-- This is no secret, only used to generate short URLs
salt :: BS.ByteString
salt = "5F,k:y]C6h<cZwN>LiEt6?^M4AEqE]|@anRG=oF<nL6M8HP2UT?Hp PZm>C4=+4>"

-- | Takes care of cleaning up 'Config' resources
shutdownApp :: Config -> IO ()
shutdownApp cfg = do
    Pool.destroyAllResources (configPool cfg)
    pure ()

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    maybeValue <- lookupEnv env
    case maybeValue of
        Nothing ->
            pure def
        Just str ->
            maybe (error $ failMsg str) pure (readMay str)
  where
    failMsg str = mconcat ["Failed to read [[", str, "]] for environment variable ", env]

corsMiddleware :: Wai.Middleware
corsMiddleware =
    Cors.cors $ Just . allowSameOriginPolicy . lookup "origin" . Wai.requestHeaders

defaultPolicy :: Cors.CorsResourcePolicy
defaultPolicy =
    Cors.CorsResourcePolicy
        { Cors.corsOrigins = Nothing
        , Cors.corsMethods = ["OPTIONS", "HEAD", "GET", "PUT", "POST", "DELETE"]
        , Cors.corsRequestHeaders =
            Cors.simpleHeaders ++ ["Authorization", "Content-Type"]
        , Cors.corsExposedHeaders = Just Cors.simpleResponseHeaders
        , Cors.corsMaxAge = Just $ 60 * 60 -- hour in seconds
        , Cors.corsVaryOrigin = True
        , Cors.corsRequireOrigin = True
        , Cors.corsIgnoreFailures = False
        }

{- | CORS policy to allow same origin, otherwise fallback to `*`.
 For requests sending credentials, CORS doesn't allow
 `Access-Control-Allow-Origin` to be set to `*`, it requires an explicit origin.
-}
allowSameOriginPolicy :: Maybe Cors.Origin -> Cors.CorsResourcePolicy
allowSameOriginPolicy origin =
    defaultPolicy
        { Cors.corsOrigins = fmap (\o -> ([o], True)) origin
        , Cors.corsRequireOrigin = False
        }
