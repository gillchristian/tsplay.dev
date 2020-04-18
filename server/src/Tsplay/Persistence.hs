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
  ( findUrlByLong,
    findUrlByShort,
    nextShortRefCounter,
    insertUrl,
  )
where

import qualified Control.Monad as Monad
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.Reader as ReaderT
import qualified Data.Bson as Bson
import qualified Database.MongoDB as Mongo
import Database.MongoDB ((=:))
import Database.MongoDB.Transport.Tls as DBTLS
import Tsplay.Types
import Prelude

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

nextShortRefCounter :: App (Maybe Int)
nextShortRefCounter = do
  res <- runDb findAndIncCounter
  case res of
    Right (Just doc) -> pure (Bson.lookup "count" doc :: Maybe Int)
    _ -> pure Nothing

findAndIncCounter :: Mongo.Action IO (Either String (Maybe Mongo.Document))
findAndIncCounter =
  Mongo.findAndModifyOpts
    (Mongo.select ["_id" =: ("counter" :: String)] "counter")
    $ upsertOptions ["$inc" =: ["count" =: (1 :: Int)]]
  where
    upsertOptions :: Mongo.Document -> Mongo.FindAndModifyOpts
    upsertOptions doc = Mongo.FamUpdate doc True True

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
