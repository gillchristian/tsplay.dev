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

module Short.Persistence (
    findAllUrls,
    findUrlByLong,
    findUrlByShort,
    findUrlByShortToVisit,
    nextShortRefCounter,
    insertUrl,
    urlsStats,
    incLinksCreated,
    incLinksVisited,
)
where

import Config (App, Config (..))
import Control.Monad (void)
import Control.Monad.Reader (asks)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Database (execDb1, execDb1_, runDb1, runDb1_, runDbN_)
import Database.PostgreSQL.Simple (Only (..))
import Prelude

import TsplayPublic.Schemas.CreatedOn
import TsplayPublic.Schemas.ShortenedUrl (ShortenedUrl (ShortenedUrl))
import TsplayPublic.Schemas.ShortenedUrl qualified as ShortenedUrl
import TsplayPublic.Schemas.Stats

fromRow :: Text -> (Text, Text, Maybe Int, Bool) -> ShortenedUrl
fromRow baseUrl (short, url, visits, expires) =
    ShortenedUrl
        { ShortenedUrl.expires = expires
        , ShortenedUrl.short = short
        , ShortenedUrl.shortened = baseUrl <> "/" <> short
        , ShortenedUrl.url = url
        , ShortenedUrl.visits = maybe 0 fromIntegral visits
        }

-- TODO pagination
findAllUrls :: App [ShortenedUrl]
findAllUrls = do
    baseUrl <- asks configBaseUrl
    fmap (fromRow baseUrl) <$> runDbN_ "select short, url, visits, expires from shortened"

findUrlByLong :: Text -> App (Maybe ShortenedUrl)
findUrlByLong url = do
    baseUrl <- asks configBaseUrl
    fmap (fromRow baseUrl) <$> runDb1 "select short, url, visits, expires from shortened where url = ?" (Only url)

findUrlByShort :: Text -> App (Maybe ShortenedUrl)
findUrlByShort short = do
    baseUrl <- asks configBaseUrl
    fmap (fromRow baseUrl) <$> runDb1 "select short, url, visits, expires from shortened where short = ?" (Only short)

findUrlByShortToVisit :: Text -> App (Maybe ShortenedUrl)
findUrlByShortToVisit short = do
    baseUrl <- asks configBaseUrl
    fmap (fromRow baseUrl)
        <$> runDb1
            "update shortened set visits = visits + 1, last_visit = now() where short = ? returning short, url, visits, expires"
            (Only short)

insertUrl :: ShortenedUrl -> App ()
insertUrl ShortenedUrl{..} =
    void $
        execDb1
            "insert into shortened (short, url, expires) values (?, ?, ?)"
            (short, url, expires)

nextShortRefCounter :: App (Maybe Int)
nextShortRefCounter =
    fmap fromOnly
        <$> runDb1_ "update hash_counter set counter = counter + 1 where id = 1 returning counter"

statsFromRow :: (Maybe Int, Maybe Int) -> Stats
statsFromRow (shortened, visited) =
    Stats (maybe 0 fromIntegral shortened) (maybe 0 fromIntegral visited)

urlsStats :: App Stats
urlsStats =
    fromMaybe (Stats 0 0)
        <$> fmap statsFromRow
        <$> runDb1_ "select links_created, links_visited from stats where id = 1"

incLinksCreated :: CreatedOn -> App ()
incLinksCreated CreatedOnClient =
    void $ execDb1_ "update stats set links_created = links_created + 1, created_on_client = created_on_client + 1 where id = 1"
incLinksCreated CreatedOnPlugin =
    void $ execDb1_ "update stats set links_created = links_created + 1, created_on_plugin = created_on_plugin + 1 where id = 1"
incLinksCreated CreatedOnApi =
    void $ execDb1_ "update stats set links_created = links_created + 1, created_on_api = created_on_api + 1 where id = 1"
incLinksCreated CreatedOnOther =
    void $ execDb1_ "update stats set links_created = links_created + 1, created_on_other = created_on_other + 1 where id = 1"

incLinksVisited :: App ()
incLinksVisited =
    void $ execDb1_ "update stats set links_visited = links_visited + 1 where id = 1"
