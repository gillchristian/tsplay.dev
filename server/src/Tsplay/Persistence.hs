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
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Database.MongoDB as DB
import Database.MongoDB ((=:))
import Database.MongoDB.Transport.Tls as DBTLS
import Tsplay.Types
import Prelude

urlsCol :: Text
urlsCol = "urls"

selectAllUrls :: DB.Query
selectAllUrls = DB.select [] urlsCol

findAllUrls :: App [ShortenedUrl]
findAllUrls = do
  docs <- runDb $ DB.rest =<< DB.find selectAllUrls
  pure $ mapMaybe fromDoc docs

findUrlByLong :: String -> App (Maybe ShortenedUrl)
findUrlByLong url = do
  doc <- runDb $ DB.findOne selectByUrl
  pure $ fromDoc =<< doc
  where
    selectByUrl = DB.select ["url" =: url] urlsCol

findUrlByShortToVisit :: String -> App (Maybe ShortenedUrl)
findUrlByShortToVisit short = do
  doc <- runDb $ DB.findAndModify selectByShort incVisits
  pure $ fromDoc =<< either (const Nothing) Just doc
  where
    selectByShort = DB.select ["short" =: short] urlsCol
    incVisits = ["$inc" =: ["visits" =: (1 :: Int)]]

-- TODO: handle case when fails to save
insertUrl :: ShortenedUrl -> App ShortenedUrl
insertUrl shortened =
  case shortenedId shortened of
    "" -> do
      newId <- runDb $ DB.insert urlsCol $ toNewDoc shortened
      pure $ shortened {shortenedId = show newId}
    _ -> do
      runDb $ DB.save urlsCol $ toDoc shortened
      pure shortened

nextShortRefCounter :: App (Maybe Int)
nextShortRefCounter = do
  res <- runDb findAndIncCounter
  pure $ either (const Nothing) (>>= Bson.lookup "count") res

findAndIncCounter :: DB.Action IO UpsertResult
findAndIncCounter = upsert selectCounter incCounter
  where
    selectCounter = DB.select ["_id" =: ("counter" :: Text)] "counter"
    incCounter = ["$inc" =: ["count" =: (1 :: Int)]]

urlsStats :: App Stats
urlsStats = do
  (visits, count) <-
    runDb $ (,) <$> DB.aggregate urlsCol [aggVisits] <*> DB.count selectAllUrls
  pure $ Stats count (lookupVisits visits)
  where
    lookupVisits = fromMaybe 0 . (Bson.lookup "visits" =<<) . listToMaybe
    sumVisits = "visits" =: ["$sum" =: ("$visits" :: Text)]
    aggVisits = ["$group" =: ["_id" =: (0 :: Int), sumVisits]]

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

type UpsertResult = Either String (Maybe DB.Document)

upsert :: (MonadIO m, MonadFail m) => DB.Query -> DB.Document -> DB.Action m UpsertResult
upsert query modify = DB.findAndModifyOpts query $ DB.FamUpdate modify True True
