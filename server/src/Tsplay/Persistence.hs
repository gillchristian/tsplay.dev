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

module Tsplay.Persistence
  ( findAllUrls,
    findUrlByLong,
    findUrlByShortToVisit,
    nextShortRefCounter,
    insertUrl,
    urlsStats,
  )
where

import qualified Control.Monad as Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.Trans.Reader as ReaderT
import qualified Data.Bson as Bson
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Database.MongoDB as DB
import Database.MongoDB ((=:))
import Database.MongoDB.Transport.Tls as DBTLS
import Tsplay.Types
import Prelude

urlsCol :: Text
urlsCol = "urls"

findAllUrls :: App [ShortenedUrl]
findAllUrls = do
  docs <- runDb $ DB.rest =<< DB.find (DB.select [] urlsCol)
  pure $ Maybe.mapMaybe fromDoc docs

findUrlByLong :: String -> App (Maybe ShortenedUrl)
findUrlByLong url = do
  doc <- runDb $ DB.findOne query
  pure $ fromDoc =<< doc
  where
    query = DB.select ["url" =: url] urlsCol

findUrlByShortToVisit :: String -> App (Maybe ShortenedUrl)
findUrlByShortToVisit short = do
  doc <- runDb $ DB.findAndModify query modify
  pure $ fromDoc =<< either (const Nothing) Just doc
  where
    query = DB.select ["short" =: short] urlsCol
    modify = ["$inc" =: ["visits" =: (1 :: Int)]]

-- TODO: handle case when fails to save
insertUrl :: ShortenedUrl -> App ShortenedUrl
insertUrl shortened@ShortenedUrl {..} =
  case shortenedId of
    "" -> do
      _id <- runDb $ DB.insert urlsCol $ toNewDoc shortened
      pure $ ShortenedUrl (show _id) shortenedShort shortenedUrl shortenedVisits
    _ -> do
      runDb $ DB.save urlsCol $ toDoc shortened
      pure ShortenedUrl {..}

nextShortRefCounter :: App (Maybe Int)
nextShortRefCounter = do
  res <- runDb findAndIncCounter
  case res of
    Right (Just doc) -> pure (Bson.lookup "count" doc :: Maybe Int)
    _ -> pure Nothing

type Result = (Either String (Maybe DB.Document))

findAndIncCounter :: DB.Action IO Result
findAndIncCounter =
  upsert query modify
  where
    query = DB.select ["_id" =: ("counter" :: Text)] "counter"
    modify = ["$inc" =: ["count" =: (1 :: Int)]]

urlsStats :: App Stats
urlsStats = do
  totalVisitsDoc <- runDb $ DB.aggregate urlsCol [visitsAggregate]
  totalDocs <- runDb $ DB.count $ DB.select [] urlsCol
  let totalVisits = Maybe.fromMaybe 0 $ Bson.lookup "totalVisits" =<< head' totalVisitsDoc
  pure $ Stats totalDocs totalVisits
  where
    visitsAggregate =
      [ "$group"
          =: ["_id" =: (0 :: Int), "totalVisits" =: ["$sum" =: ("$visits" :: Text)]]
      ]

head' :: [a] -> Maybe a
head' [] = Nothing
head' (a : _) = Just a

-- "_id" := null,

fromDoc :: DB.Document -> Maybe ShortenedUrl
fromDoc doc =
  ShortenedUrl . show
    <$> (Bson.lookup "_id" doc :: Maybe Bson.ObjectId)
    <*> Bson.lookup "short" doc
    <*> Bson.lookup "url" doc
    <*> Bson.lookup "visits" doc

toDoc :: ShortenedUrl -> DB.Document
toDoc shortened = ("_id" =: _id shortened) : toNewDoc shortened

toNewDoc :: ShortenedUrl -> DB.Document
toNewDoc ShortenedUrl {..} =
  [ "short" =: shortenedShort,
    "url" =: shortenedUrl,
    "visits" =: shortenedVisits
  ]

_id :: ShortenedUrl -> DB.Value
_id = Bson.ObjId . read . shortenedId

runDb :: DB.Action IO a -> App a
runDb action = do
  dbConf <- appDatabase . conf <$> ReaderT.ask
  env <- appEnv . conf <$> ReaderT.ask
  pipe <- liftIO $ case env of
    Production -> DBTLS.connect (dbHostname dbConf) (DB.PortNumber 27017)
    Development -> DB.connect $ DB.host $ dbHostname dbConf
  Monad.void
    $ DB.access pipe DB.master "admin"
    $ DB.auth (dbUser dbConf) (dbPassword dbConf)
  result <- liftIO $ DB.access pipe DB.master (dbName dbConf) action
  liftIO $ DB.close pipe
  pure result

upsert :: (MonadIO m, MonadFail m) => DB.Query -> DB.Document -> DB.Action m Result
upsert query modify =
  DB.findAndModifyOpts query $ DB.FamUpdate modify True True
