{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( runServer,
  )
where

import qualified Control.Monad as Monad
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.Reader as ReaderT
import qualified Data.Aeson as Json
import qualified Data.Bson as Bson
import qualified Data.ByteString.Char8 as B
import Data.Default.Class (Default (def))
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Database.MongoDB as Mongo
import Database.MongoDB ((=:))
import Database.MongoDB.Transport.Tls as DBTLS
import GHC.Generics
import Network.HTTP.Types.Status (status302)
import qualified Network.URI as URI
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors)
import qualified Network.Wai.Middleware.RequestLogger as Log
import Network.Wai.Middleware.RequestLogger.JSON (formatAsJSON)
import Servant ((:<|>) (..), (:>), Context ((:.), EmptyContext))
import qualified Servant
import qualified System.Environment as Sys
import qualified System.Exit as Exit
import Text.Casing (camel)
import qualified Web.Hashids as Hashids
import Prelude

type App = ReaderT.ReaderT AppEnv Servant.Handler

runDb :: Mongo.Action IO a -> App a
runDb action = do
  dbConf <- appDatabase . conf <$> ReaderT.ask
  env <- appEnv . conf <$> ReaderT.ask
  pipe <- liftIO $ case env of
    Production -> DBTLS.connect (dbHostname dbConf) (Mongo.PortNumber 27017)
    Development -> Mongo.connect $ Mongo.host $ dbHostname dbConf
  Monad.void $ Mongo.access pipe Mongo.master "admin" $ Mongo.auth (dbUser dbConf) (dbPassword dbConf)
  result <- liftIO $ Mongo.access pipe Mongo.master (dbName dbConf) action
  liftIO $ Mongo.close pipe
  pure result

findUrlByLong :: String -> App (Maybe ShortenedUrl)
findUrlByLong url = do
  doc <- runDb $ Mongo.findOne (Mongo.select ["url" =: url] "urls")
  pure $ fromDoc doc

findUrlByShort :: String -> App (Maybe ShortenedUrl)
findUrlByShort short = do
  doc <- runDb (Mongo.findAndModify (Mongo.select ["short" =: short] "urls") ["$inc" =: ["visits" =: (1 :: Int)]])
  pure $ fromDoc $ either (const Nothing) Just doc

fromDoc :: Maybe Mongo.Document -> Maybe ShortenedUrl
fromDoc Nothing = Nothing
fromDoc (Just doc) =
  ShortenedUrl . show
    <$> (Bson.lookup "_id" doc :: Maybe Bson.ObjectId)
    <*> Bson.lookup "short" doc
    <*> Bson.lookup "url" doc
    <*> Bson.lookup "visits" doc

toDoc :: ShortenedUrl -> Mongo.Document
toDoc ShortenedUrl {..} =
  [ "_id" =: Bson.ObjId (read shortenedId),
    "short" =: shortenedShort,
    "url" =: shortenedUrl,
    "visits" =: shortenedVisits
  ]

-- TODO: handle case when fails to save
insertUrl :: ShortenedUrl -> App ShortenedUrl
insertUrl shortened@ShortenedUrl {..} =
  case shortenedId of
    "" -> do
      _id <- runDb $ Mongo.insert "urls" ["short" =: shortenedShort, "url" =: shortenedUrl, "visits" =: shortenedVisits]
      pure $ ShortenedUrl (show _id) shortenedShort shortenedUrl shortenedVisits
    _ -> do
      runDb $ Mongo.save "items" $ toDoc shortened
      pure ShortenedUrl {..}

upsertOptions :: Mongo.Document -> Mongo.FindAndModifyOpts
upsertOptions doc = Mongo.FamUpdate doc True True

findAndIncCounter :: Mongo.Action IO (Either String (Maybe Mongo.Document))
findAndIncCounter =
  Mongo.findAndModifyOpts
    (Mongo.select ["_id" =: ("counter" :: String)] "counter")
    $ upsertOptions ["$inc" =: ["count" =: (1 :: Int)]]

nextCounter :: App (Maybe Int)
nextCounter = do
  res <- runDb findAndIncCounter
  case res of
    Right (Just doc) -> pure (Bson.lookup "count" doc :: Maybe Int)
    _ -> pure Nothing

-- This is no secret, only used to generate short URLs
salt :: B.ByteString
salt = "5F,k:y]C6h<cZwN>LiEt6?^M4AEqE]|@anRG=oF<nL6M8HP2UT?Hp PZm>C4=+4>"

-- SERVER ---
runServer :: IO ()
runServer = do
  let loggerSettings =
        def {Log.outputFormat = Log.CustomOutputFormatWithDetails formatAsJSON}
  logger <- Log.mkRequestLogger loggerSettings
  (env, port) <- loadAppEnv
  let client = B.pack $ appClientUrl $ conf env
  startMsg port
  Warp.run port $ appMiddleware logger client $ app (env :. EmptyContext) env

loadAppEnv :: IO (AppEnv, Int)
loadAppEnv = do
  mbConf <- Json.decodeFileStrict ".env.json"
  env <- case mbConf of
    Just conf -> pure $ AppEnv conf $ Hashids.hashidsMinimum salt 6
    Nothing -> Exit.die "Invalid configuration"
  port <- maybe 8080 read <$> Sys.lookupEnv "PORT"
  pure (env, port)

appMiddleware :: Wai.Middleware -> B.ByteString -> Wai.Middleware
appMiddleware logger client = corsified . logger . rootMiddleware client

startMsg :: Int -> IO ()
startMsg port =
  putStrLn $ "{ \"init\": \"Server starting in port " ++ show port ++ "\" }"

rootMiddleware :: B.ByteString -> Wai.Middleware
rootMiddleware client =
  Wai.ifRequest ifRoot $ Wai.modifyResponse (addRedirectHeaders client . set302)

set302 :: Wai.Response -> Wai.Response
set302 = Wai.mapResponseStatus $ const status302

ifRoot :: Wai.Request -> Bool
ifRoot = null . Wai.pathInfo

addRedirectHeaders :: B.ByteString -> Wai.Response -> Wai.Response
addRedirectHeaders client =
  Wai.mapResponseHeaders $ (:) ("Location", client)

app :: Servant.Context '[AppEnv] -> AppEnv -> Servant.Application
app ctx env =
  Servant.serveWithContext tsplayAPI ctx $
    Servant.hoistServerWithContext tsplayAPI tsplayCtx (`ReaderT.runReaderT` env) tsplayServer

type TsplayAPI = CreateRoute :<|> VisitRoute -- :<|> HomeRoute

type CreateRoute =
  "create"
    :> Servant.ReqBody '[Servant.JSON] CreateBody
    :> Servant.Post '[Servant.JSON] CreateResponse

type VisitRoute =
  Servant.Capture "short" String
    :> Servant.Get '[Servant.JSON] ()

-- type HomeRoute = "" :> Servant.Get '[HTML] ()

tsplayAPI :: Servant.Proxy TsplayAPI
tsplayAPI = Servant.Proxy

tsplayCtx :: Servant.Proxy '[AppEnv]
tsplayCtx = Servant.Proxy

tsplayServer :: Servant.ServerT TsplayAPI App
tsplayServer =
  createHandler :<|> visitHandler

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
      seed <- maybe (Servant.throwError Servant.err500) pure =<< nextCounter
      short <- B.unpack . flip Hashids.encode seed . hashidsCtx <$> ReaderT.ask
      Monad.void $ insertUrl $ ShortenedUrl "" short createUrl 0
      pure $ CreateResponse (baseUrl ++ "/" ++ short) -- TODO: return 201
    Just ShortenedUrl {..} ->
      pure $ CreateResponse (baseUrl ++ "/" ++ shortenedShort)

visitHandler :: String -> App ()
visitHandler short = do
  mbUrl <- findUrlByShort short
  case mbUrl of
    Just ShortenedUrl {..} ->
      redirectTo shortenedUrl
    Nothing ->
      redirectTo =<< appClientUrl . conf <$> ReaderT.ask

redirectTo :: String -> App ()
redirectTo url =
  Servant.throwError $
    Servant.err302 {Servant.errHeaders = [("Location", B.pack url)]}

-- -- Types ---

dropLabelPrefix :: String -> Json.Options
dropLabelPrefix prefix =
  Json.defaultOptions {Json.fieldLabelModifier = camel . stripPrefix prefix}

camelTags :: Json.Options
camelTags =
  Json.defaultOptions {Json.constructorTagModifier = camel}

data CreateBody
  = CreateBody
      { createUrl :: String,
        createShort :: Maybe String
      }
  deriving stock (Generic, Show)

instance Json.ToJSON CreateBody where
  toJSON = Json.genericToJSON $ dropLabelPrefix "create"

instance Json.FromJSON CreateBody where
  parseJSON = Json.genericParseJSON $ dropLabelPrefix "create"

newtype CreateResponse
  = CreateResponse
      { createShortened :: String
      }
  deriving stock (Generic, Show)

instance Json.ToJSON CreateResponse where
  toJSON = Json.genericToJSON $ dropLabelPrefix "create"

instance Json.FromJSON CreateResponse where
  parseJSON = Json.genericParseJSON $ dropLabelPrefix "create"

data DbConfig
  = DbConfig
      { dbHostname :: String,
        dbName :: Text,
        dbUser :: Text,
        dbPassword :: Text
      }
  deriving stock (Generic, Show)

instance Json.FromJSON DbConfig where
  parseJSON = Json.genericParseJSON $ dropLabelPrefix "db"

data Environment
  = Production
  | Development
  deriving stock (Generic, Show)

instance Json.FromJSON Environment where
  parseJSON = Json.genericParseJSON camelTags

instance Json.ToJSON Environment where
  toJSON = Json.genericToJSON camelTags

data AppConfig
  = AppConfig
      { appDatabase :: DbConfig,
        appBaseUrl :: String,
        appClientUrl :: String,
        appEnv :: Environment
      }
  deriving stock (Generic, Show)

instance Json.FromJSON AppConfig where
  parseJSON = Json.genericParseJSON $ dropLabelPrefix "app"

data AppEnv
  = AppEnv
      { conf :: AppConfig,
        hashidsCtx :: Hashids.HashidsContext
      }

data ShortenedUrl
  = ShortenedUrl
      { shortenedId :: String,
        shortenedShort :: String,
        shortenedUrl :: String,
        shortenedVisits :: Int
      }
  deriving stock (Generic, Show)

instance Json.ToJSON ShortenedUrl where
  toJSON = Json.genericToJSON $ dropLabelPrefix "shortened"

instance Json.FromJSON ShortenedUrl where
  parseJSON = Json.genericParseJSON $ dropLabelPrefix "shortened"

-- -- Utils ---

stripPrefix :: Eq a => [a] -> [a] -> [a]
stripPrefix prefix str = Maybe.fromMaybe str $ List.stripPrefix prefix str

-- -- CORS ---

corsified :: Wai.Middleware
corsified = cors (const $ Just appCorsResourcePolicy)

-- TODO: allow only from clientUrl
appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy = CorsResourcePolicy
  { corsOrigins = Nothing,
    corsMethods = ["OPTIONS", "GET", "PUT", "POST"],
    corsRequestHeaders = ["Authorization", "Content-Type"],
    corsExposedHeaders = Nothing,
    corsMaxAge = Nothing,
    corsVaryOrigin = False,
    corsRequireOrigin = False,
    corsIgnoreFailures = False
  }
