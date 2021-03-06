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

module Api.Short
  ( ShortAPI,
    shortApi,
    shortServer,
    isValidShort,
  )
where

import Api.Short.Models
import Api.Short.Persistence
import Config (AppT (..), Config (..))
import qualified Control.Monad as Monad
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (asks)
import qualified Data.Char as Char
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import General.Util ((<&>), (<|>))
import qualified Network.URI as URI
import Servant ((:<|>) (..), (:>))
import qualified Servant
import qualified Web.Hashids as Hashids
import Prelude

type ShortAPI =
  HealthzRoute
    :<|> CreateRoute
    :<|> ListAllRoute
    :<|> StatsRoute
    :<|> VisitRoute

type HealthzRoute =
  "healthz"
    :> Servant.Get '[Servant.PlainText] Text

type CreateRoute =
  "api" :> "short"
    :> Servant.ReqBody '[Servant.JSON] CreateBody
    :> Servant.Post '[Servant.JSON] CreateResponse

type ListAllRoute =
  "api" :> "short"
    :> Servant.Get '[Servant.JSON] [ShortenedUrl]

type StatsRoute =
  "api" :> "short" :> "stats"
    :> Servant.Get '[Servant.JSON] Stats

type VisitRoute =
  Servant.Capture "short" Text
    :> Servant.Get '[Servant.JSON] ()

shortServer :: MonadIO m => Servant.ServerT ShortAPI (AppT m)
shortServer =
  healthzHandler
    :<|> createHandler
    :<|> listAllHandler
    :<|> statsHandler
    :<|> visitHandler

shortApi :: Servant.Proxy ShortAPI
shortApi = Servant.Proxy

healthzHandler :: MonadIO m => AppT m Text
healthzHandler = pure "200 Ok"

isValidURL :: Text -> Bool
isValidURL =
  (URI.isURI . Text.unpack)
    <&> Text.isPrefixOf "https://www.typescriptlang.org/"
    <|> Text.isPrefixOf "https://www.staging-typescript.org/"

shorterThan :: Int -> Text -> Bool
shorterThan n = (< n) . Text.length

longerThan :: Int -> Text -> Bool
longerThan n = (> n) . Text.length

isValidChar :: Char -> Bool
isValidChar = Char.isAlpha <|> Char.isDigit <|> isUnderscore <|> isHyphen

isUnderscore :: Char -> Bool
isUnderscore '_' = True
isUnderscore _ = False

isHyphen :: Char -> Bool
isHyphen '-' = True
isHyphen _ = False

-- Char.isAlphaNum includes every numeric character, we only want ASCII ones:
-- > Note that numeric digits outside the ASCII range, as well as numeric characters
-- > which aren't digits, are selected by this function but not by isDigit.
-- https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Char.html#v:isAlphaNum
isAlphaNum :: Char -> Bool
isAlphaNum = Char.isAlpha <|> Char.isDigit

headIs :: (Char -> Bool) -> Text -> Bool
headIs _ "" = False
headIs f t = f $ Text.head t

lastIs :: (Char -> Bool) -> Text -> Bool
lastIs _ "" = False
lastIs f t = f $ Text.last t

isValidShort :: Text -> Bool
isValidShort =
  longerThan 4
    <&> shorterThan 31
    <&> Text.all isValidChar
    <&> headIs Char.isAlpha
    <&> lastIs isAlphaNum

createHandler :: MonadIO m => CreateBody -> AppT m CreateResponse
createHandler CreateBody {..} = do
  Monad.unless (isValidURL createUrl) $ Servant.throwError Servant.err400
  case createShort of
    Just short -> do
      Monad.unless (isValidShort short) $ Servant.throwError Servant.err400
      createWithCustomShort createUrl short createCreatedOn createExpires
    Nothing -> createWithRandomShort createUrl createCreatedOn createExpires

createWithCustomShort :: MonadIO m => Text -> Text -> Maybe CreatedOn -> Maybe Bool -> AppT m CreateResponse
createWithCustomShort createUrl short createCreatedOn createExpires = do
  mbByShort <- findUrlByShort short
  mbByLong <- findUrlByLong createUrl
  case (mbByShort, mbByLong) of
    (Nothing, Nothing) -> do
      Monad.void $ insertUrl $ ShortenedUrl short createUrl 0 (fromMaybe False createExpires)
      incLinksCreated $ fromMaybe Other createCreatedOn
      respondOr400 short True
    (Just byShort, Just byLong) ->
      let exists = shortenedUrl byShort == createUrl && short == shortenedShort byLong
       in respondOr400 short exists
    (Nothing, Just byLong) -> respondOr400 short $ short == shortenedShort byLong
    (Just byShort, Nothing) -> respondOr400 short $ createUrl == shortenedUrl byShort
  where
    respondOr400 :: MonadIO m => Text -> Bool -> AppT m CreateResponse
    respondOr400 short' True = do
      baseUrl <- asks configBaseUrl
      pure $ CreateResponse (baseUrl <> "/" <> short')
    respondOr400 _ False = Servant.throwError Servant.err400

createWithRandomShort :: MonadIO m => Text -> Maybe CreatedOn -> Maybe Bool -> AppT m CreateResponse
createWithRandomShort createUrl createCreatedOn createExpires = do
  mbShortened <- findUrlByLong createUrl
  baseUrl <- asks configBaseUrl
  case mbShortened of
    Nothing -> do
      seed <- maybe (Servant.throwError Servant.err500) pure =<< nextShortRefCounter
      short <- asks (decodeUtf8 . flip Hashids.encode seed . configHashidsCtx)
      Monad.void $ insertUrl $ ShortenedUrl short createUrl 0 (fromMaybe False createExpires)
      incLinksCreated $ fromMaybe Other createCreatedOn
      pure $ CreateResponse (baseUrl <> "/" <> short) -- TODO: return 201
    Just ShortenedUrl {..} ->
      pure $ CreateResponse (baseUrl <> "/" <> shortenedShort)

-- TODO: pagination + total count
listAllHandler :: MonadIO m => AppT m [ShortenedUrl]
listAllHandler = findAllUrls

statsHandler :: MonadIO m => AppT m Stats
statsHandler = urlsStats

visitHandler :: MonadIO m => Text -> AppT m ()
visitHandler short = do
  mbUrl <- findUrlByShortToVisit short
  case mbUrl of
    Just ShortenedUrl {..} -> do
      incLinksVisited
      redirectTo shortenedUrl
    Nothing ->
      redirectTo =<< asks configClientUrl

redirectTo :: MonadIO m => Text -> AppT m ()
redirectTo url =
  Servant.throwError $
    Servant.err302 {Servant.errHeaders = [("Location", encodeUtf8 url)]}
