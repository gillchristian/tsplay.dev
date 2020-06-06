{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Api.Short.Persistence
  ( findAllUrls,
    findUrlByLong,
    findUrlByShortToVisit,
    nextShortRefCounter,
    insertUrl,
    urlsStats,
    incLinksCreated,
    incLinksVisited,
  )
where

import Api.Short.Models
import Config (AppT (..))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Database (execDb1, execDb1_, runDb1, runDb1_, runDbN_)
import Database.PostgreSQL.Simple (Only (..))
import Prelude

findAllUrls :: MonadIO m => AppT m [ShortenedUrl]
findAllUrls = runDbN_ "select short,url,visits,expires from shortened" -- TODO pagination

findUrlByLong :: MonadIO m => Text -> AppT m (Maybe ShortenedUrl)
findUrlByLong url =
  runDb1 "select short,url,visits,expires from shortened where url = ?" (Only url)

findUrlByShortToVisit :: MonadIO m => Text -> AppT m (Maybe ShortenedUrl)
findUrlByShortToVisit short =
  runDb1
    "update shortened set visits = visits + 1, last_visit = now() where short = ? returning short,url,visits,expires"
    (Only short)

insertUrl :: MonadIO m => ShortenedUrl -> AppT m ()
insertUrl ShortenedUrl {..} =
  void $
    execDb1
      "insert into shortened (short,url,expires) values (?,?,?)"
      (shortenedShort, shortenedUrl, shortenedExpires)

nextShortRefCounter :: MonadIO m => AppT m (Maybe Int)
nextShortRefCounter =
  fmap fromOnly
    <$> runDb1_ "update hash_counter set counter = counter + 1 where id = 1 returning counter"

urlsStats :: MonadIO m => AppT m Stats
urlsStats =
  fromMaybe (Stats 0 0)
    <$> runDb1_ "select links_created,links_visited from stats where id = 1"

incLinksCreated :: MonadIO m => CreatedOn -> AppT m ()
incLinksCreated Client =
  void $ execDb1_ "update stats set links_created = links_created + 1, created_on_client = created_on_client + 1 where id = 1"
incLinksCreated Plugin =
  void $ execDb1_ "update stats set links_created = links_created + 1, created_on_plugin = created_on_plugin + 1 where id = 1"
incLinksCreated Api =
  void $ execDb1_ "update stats set links_created = links_created + 1, created_on_api = created_on_api + 1 where id = 1"
incLinksCreated Other =
  void $ execDb1_ "update stats set links_created = links_created + 1, created_on_other = created_on_other + 1 where id = 1"

incLinksVisited :: MonadIO m => AppT m ()
incLinksVisited =
  void $ execDb1_ "update stats set links_visited = links_visited + 1 where id = 1"
