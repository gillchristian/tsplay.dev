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
  )
where

import Api.Short.Models
import Config (AppT (..))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Database (execDb1, runDb1, runDb1_, runDbN_)
import Database.PostgreSQL.Simple (Only (..))
import Prelude

findAllUrls :: MonadIO m => AppT m [ShortenedUrl]
findAllUrls = runDbN_ "select short,url,visits from shortened"

findUrlByLong :: MonadIO m => Text -> AppT m (Maybe ShortenedUrl)
findUrlByLong url =
  runDb1 "select short,url,visits from shortened where url = ?" (Only url)

findUrlByShortToVisit :: MonadIO m => Text -> AppT m (Maybe ShortenedUrl)
findUrlByShortToVisit short =
  runDb1
    "update shortened set visits = visits + 1 where short = ? returning short,url,visits"
    (Only short)

insertUrl :: MonadIO m => ShortenedUrl -> AppT m ()
insertUrl ShortenedUrl {..} =
  void $
    execDb1
      "insert into shortened (short,url) values (?,?)"
      (shortenedShort, shortenedUrl)

nextShortRefCounter :: MonadIO m => AppT m (Maybe Int)
nextShortRefCounter =
  fmap fromOnly
    <$> runDb1_ "update hash_counter set counter = counter + 1 where id = 1 returning counter"

urlsStats :: MonadIO m => AppT m Stats
urlsStats =
  -- TODO: use a separate table instead
  fromMaybe (Stats 0 0) <$> runDb1_ "select count(*), sum(visits) from shortened"
