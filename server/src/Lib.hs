{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

-- TODO use qualified

import Codec.Binary.UTF8.String as UTF8
import Control.Concurrent (forkIO)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader hiding (ask)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader
import qualified Data.Aeson as Json
import Data.Aeson ((.:))
import qualified Data.Bson as Bson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BC
import Data.Default.Class (Default (def))
import Data.Foldable (fold)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Database.MongoDB as Mongo
import Database.MongoDB ((=:))
import GHC.Generics
import Network.HTTP.Types.Status (status302)
import Network.URI (URI)
import qualified Network.URI as URI
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors)
import qualified Network.Wai.Middleware.RequestLogger as Log
import Network.Wai.Middleware.RequestLogger.JSON (formatAsJSON)
import qualified Network.Wreq as Wreq
import Servant ((:<|>) (..), (:>), Context ((:.), EmptyContext))
import qualified Servant
import Servant.Server.Internal.ServerError (ServerError)
import qualified System.Environment as Sys
import qualified System.Exit as Exit
import Text.Casing (camel)
import qualified Web.Hashids as Hashids

type App = ReaderT AppEnv Servant.Handler

runDb :: Mongo.Action IO a -> App a
runDb action = do
  -- TODO: check this pipeline
  -- TODO: can I have a pool of connections in de Reader instead?
  dbConf <- appDatabase . conf <$> ask
  pipe <- liftIO $ Mongo.connect (Mongo.host (dbHostname dbConf))
  -- Have to check if it isn't authorized
  _ <- lift $ Mongo.access pipe Mongo.master (dbName dbConf) $ Mongo.auth (dbUser dbConf) (dbPassword dbConf)
  result <- liftIO $ Mongo.access pipe Mongo.master (dbName dbConf) action
  liftIO $ Mongo.close pipe
  return result

findUrlById :: String -> App (Maybe ShortenedUrl)
findUrlById _id = do
  doc <- runDb $ Mongo.findOne (Mongo.select ["short" =: (Bson.ObjId $ read _id)] "urls")
  pure $ fromDoc doc

findUrlByLong :: String -> App (Maybe ShortenedUrl)
findUrlByLong url = do
  doc <- runDb $ Mongo.findOne (Mongo.select ["url" =: url] "urls")
  pure $ fromDoc doc

findUrlByShort :: String -> App (Maybe ShortenedUrl)
findUrlByShort short = do
  doc <- runDb (Mongo.findAndModify (Mongo.select ["short" =: short] "urls") ["$inc" =: ["visits" =: 1]])
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

removeUrl :: String -> App ()
removeUrl _id =
  runDb
    $ Mongo.delete
    $ Mongo.select ["_id" =: Bson.ObjId (read _id)] "urls"

upsertOptions :: Mongo.Document -> Mongo.FindAndModifyOpts
upsertOptions doc = Mongo.FamUpdate doc True True

findAndIncCounter :: Mongo.Action IO (Either String (Maybe Mongo.Document))
findAndIncCounter =
  Mongo.findAndModifyOpts
    (Mongo.select ["_id" =: "counter"] "counter")
    $ upsertOptions ["$inc" =: ["count" =: 1]]

nextCounter :: App (Maybe Int)
nextCounter = do
  res <- runDb findAndIncCounter
  case res of
    Right (Just doc) -> pure (Bson.lookup "count" doc :: Maybe Int)
    _ -> pure Nothing

-- We aren't hardcoding anything secret here, only used to generate short URLs
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
  port <- maybe 9000 read <$> Sys.lookupEnv "PORT"
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
    Servant.hoistServerWithContext tsplayAPI tsplayCtx (`runReaderT` env) tsplayServer

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
  createHandler :<|> visitHandler -- :<|> homeHandler

isValidURL :: String -> Bool
isValidURL = (&&) <$> URI.isURI <*> List.isInfixOf "typescriptlang.org"

-- homeHandler :: App ()
-- homeHandler = do
--   client <- clientUrl <$> ask
--   Servant.throwError $
--     Servant.err302 {Servant.errHeaders = [("Location", client)]}

createHandler :: CreateBody -> App CreateResponse
createHandler CreateBody {..} = do
  -- TODO: use provided 'short'
  unless (isValidURL createUrl) $ Servant.throwError Servant.err400
  mbShortened <- findUrlByLong createUrl
  baseUrl <- appBaseUrl . conf <$> ask
  case mbShortened of
    Nothing -> do
      seed <- maybe (Servant.throwError Servant.err500) pure =<< nextCounter
      short <- B.unpack . flip Hashids.encode seed . hashidsCtx <$> ask
      void $ insertUrl $ ShortenedUrl "" short createUrl 0
      pure $ CreateResponse (baseUrl ++ "/" ++ short) -- TODO: return 201
    Just ShortenedUrl {..} ->
      pure $ CreateResponse (baseUrl ++ "/" ++ shortenedShort)

visitHandler :: String -> App ()
visitHandler short = do
  mbUrl <- findUrlByShort short
  case mbUrl of
    Just ShortenedUrl {..} ->
      Servant.throwError $ Servant.err302 {Servant.errHeaders = [("Location", B.pack shortenedUrl)]}
    Nothing -> Servant.throwError Servant.err404

-- -- Types ---

dropLabelPrefix :: String -> Json.Options
dropLabelPrefix prefix =
  Json.defaultOptions {Json.fieldLabelModifier = camel . stripPrefix prefix}

data CreateBody
  = CreateBody
      { createUrl :: String,
        createShort :: Maybe String
      }
  deriving (Generic, Show)

instance Json.ToJSON CreateBody where
  toJSON = Json.genericToJSON $ dropLabelPrefix "create"

instance Json.FromJSON CreateBody where
  parseJSON = Json.genericParseJSON $ dropLabelPrefix "create"

newtype CreateResponse
  = CreateResponse
      { createShortened :: String
      }
  deriving (Generic, Show)

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
  deriving (Generic, Show)

instance Json.FromJSON DbConfig where
  parseJSON = Json.genericParseJSON $ dropLabelPrefix "db"

data AppConfig
  = AppConfig
      { appDatabase :: DbConfig,
        appBaseUrl :: String,
        appClientUrl :: String
      }
  deriving (Generic, Show)

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
  deriving (Generic, Show)

instance Json.ToJSON ShortenedUrl where
  toJSON = Json.genericToJSON $ dropLabelPrefix "shortened"

instance Json.FromJSON ShortenedUrl where
  parseJSON = Json.genericParseJSON $ dropLabelPrefix "shortened"

-- -- Utils ---

listJoin :: [a] -> [[a]] -> [a]
listJoin what = fold . List.intersperse what

stripPrefix :: Eq a => [a] -> [a] -> [a]
stripPrefix prefix str = Maybe.fromMaybe str $ List.stripPrefix prefix str

-- -- CORS ---

allowCsrf :: Wai.Middleware
allowCsrf = addHeaders [("Access-Control-Allow-Headers", "x-csrf-token,authorization")]

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
-- data HTML

-- instance Servant.Accept HTML where
--   contentType _ = "text" // "html" /: ("charset", "utf-8")

-- instance Show a => Servant.MimeRender HTML a where
--   mimeRender _ val = BC.pack $ UTF8.encode $ show val
