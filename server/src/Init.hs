{-# LANGUAGE OverloadedStrings #-}

module Init
  ( runApp,
  )
where

import Api (app)
import Config
  ( Config (..),
    Environment (..),
    makePool,
    setLogger,
  )
import Control.Concurrent (killThread)
import Control.Exception (bracket)
import Control.Monad (void)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Pool as Pool
import Data.Text.Encoding (encodeUtf8)
import Database (initializeDatabase)
import qualified Katip
import Logger (defaultLogEnv)
import Network.HTTP.Types.Status (status302)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors)
import Safe (readMay)
import System.Environment (lookupEnv)
import System.Remote.Monitoring
  ( forkServer,
    serverThreadId,
  )
import qualified Web.Hashids as Hashids
import Prelude

-- | An action that creates a WAI 'Application' together with its resources,
-- runs it, and tears it down on exit
runApp :: IO ()
runApp = bracket acquireConfig shutdownApp runApp'
  where
    runApp' config = run (configPort config) =<< initialize config

-- | The 'initialize' function accepts the required environment information,
-- initializes the WAI 'Application' and returns it
initialize :: Config -> IO Wai.Application
initialize cfg = do
  let logger = setLogger (configEnv cfg)
  Pool.withResource (configPool cfg) $ initializeDatabase "./database/migrations"
  pure
    . logger
    . corsified
    . rootRedirectTo (encodeUtf8 $ configClientUrl cfg)
    . app
    $ cfg

-- | Allocates resources for 'Config'
acquireConfig :: IO Config
acquireConfig = do
  port <- lookupSetting "PORT" 9000
  env <- lookupSetting "ENV" Development
  baseUrl <- lookupSetting "BASE_URL" "http://localhost:9000"
  clientUrl <- lookupSetting "CLIENT_URL" "http://localhost:3000"
  logEnv <- defaultLogEnv
  pool <- makePool env logEnv
  ekgServer <- forkServer "localhost" =<< lookupSetting "PORT_EKG" 8000
  pure
    Config
      { configPool = pool,
        configEnv = env,
        configLogEnv = logEnv,
        configPort = port,
        configEkgServer = serverThreadId ekgServer,
        configHashidsCtx = Hashids.hashidsMinimum salt 6,
        configBaseUrl = baseUrl,
        configClientUrl = clientUrl
      }

-- This is no secret, only used to generate short URLs
salt :: BS.ByteString
salt = "5F,k:y]C6h<cZwN>LiEt6?^M4AEqE]|@anRG=oF<nL6M8HP2UT?Hp PZm>C4=+4>"

-- | Takes care of cleaning up 'Config' resources
shutdownApp :: Config -> IO ()
shutdownApp cfg = do
  void $ Katip.closeScribes (configLogEnv cfg)
  Pool.destroyAllResources (configPool cfg)
  killThread (configEkgServer cfg)
  pure ()

-- | Looks up a setting in the environment, with a provided default, and
-- 'read's that information into the inferred type.
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

-- | A Wai Middleware to redirect to a given URL on requests to the root path
rootRedirectTo :: BS.ByteString -> Wai.Middleware
rootRedirectTo url =
  Wai.ifRequest isRoot $ Wai.modifyResponse (addRedirectHeaders url . set302)

set302 :: Wai.Response -> Wai.Response
set302 = Wai.mapResponseStatus $ const status302

isRoot :: Wai.Request -> Bool
isRoot = null . Wai.pathInfo

addRedirectHeaders :: BS.ByteString -> Wai.Response -> Wai.Response
addRedirectHeaders url = Wai.mapResponseHeaders $ (:) ("Location", url)

corsified :: Wai.Middleware
corsified = cors (const $ Just corsResourcePolicy)

-- TODO: allow only from client's url
corsResourcePolicy :: CorsResourcePolicy
corsResourcePolicy =
  CorsResourcePolicy
    { corsOrigins = Nothing,
      corsMethods = ["OPTIONS", "GET", "PUT", "POST"],
      corsRequestHeaders = ["Authorization", "Content-Type"],
      corsExposedHeaders = Nothing,
      corsMaxAge = Nothing,
      corsVaryOrigin = False,
      corsRequireOrigin = False,
      corsIgnoreFailures = False
    }
