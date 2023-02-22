{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Short.Api (createHandler, visitHandler, listAllHandler) where

import Config (App, Config (..))
import Control.Monad (unless, void)
import Control.Monad.Except (ExceptT, lift, runExceptT, throwError)
import Control.Monad.Reader (asks)
import Data.Char qualified as Char
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Network.URI qualified as URI
import Short.Persistence
import TsplayPublic.Response.CreateShort (CreateShortResponse (..))
import TsplayPublic.Response.ListAllShorts (ListAllShortsResponse (..))
import TsplayPublic.Response.VisitShortened (VisitShortenedResponse (..))
import TsplayPublic.Schemas.CreateRequest (CreateRequest (..))
import TsplayPublic.Schemas.CreatedOn (CreatedOn (..))
import TsplayPublic.Schemas.Error (Error (..))
import TsplayPublic.Schemas.ShortenedUrl (ShortenedUrl (ShortenedUrl))
import TsplayPublic.Schemas.ShortenedUrl qualified as ShortenedUrl
import Util ((<&>), (<|>))
import Web.Hashids qualified as Hashids
import Prelude

-- TODO: handler error
--       pagination
listAllHandler :: App ListAllShortsResponse
listAllHandler = ListAllShortsResponse200 <$> findAllUrls

visitHandler :: Text -> App VisitShortenedResponse
visitHandler short = do
  mbUrl <- findUrlByShortToVisit short
  case mbUrl of
    Just ShortenedUrl{ShortenedUrl.url = url} -> do
      incLinksVisited
      pure $ VisitShortenedResponse302 url
    Nothing -> do
      client <- asks configClientUrl
      pure $ VisitShortenedResponse302 client

isValidTypeScriptURL :: Text -> Bool
isValidTypeScriptURL url =
  case Text.pack <$> URI.uriRegName <$> (URI.uriAuthority =<< URI.parseURI (Text.unpack url)) of
    Nothing -> False
    Just host ->
      Text.isPrefixOf "www.typescriptlang.org"
        <|> Text.isPrefixOf "www.staging-typescript.org"
        $ host

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

data CreateError
  = Err400 Error
  | Err500 Error

errorToResponse :: CreateError -> CreateShortResponse
errorToResponse (Err400 err) = CreateShortResponse400 err
errorToResponse (Err500 err) = CreateShortResponse500 err

-- TODO: the short should just the short, not the full URL

createHandler :: CreateRequest -> App CreateShortResponse
createHandler CreateRequest{url, createdOn, expires, short = mbShort} = do
  result <- runExceptT $ do
    unless (isValidTypeScriptURL url) $ throwError $ Err400 $ Error "Invalid URL"
    case mbShort of
      Just short -> do
        unless (isValidShort short) $ throwError $ Err400 $ Error "Invalid custom short"
        createWithCustomShort url short createdOn expires
      Nothing -> createWithRandomShort url createdOn expires
  pure $ either errorToResponse CreateShortResponse201 result

createWithCustomShort :: Text -> Text -> Maybe CreatedOn -> Maybe Bool -> ExceptT CreateError App ShortenedUrl
createWithCustomShort createUrl short createCreatedOn createExpires = do
  mbByShort <- lift $ findUrlByShort short
  mbByLong <- lift $ findUrlByLong createUrl
  baseUrl <- asks configBaseUrl
  let shortened =
        ShortenedUrl
          { ShortenedUrl.expires = False
          , ShortenedUrl.short = short
          , ShortenedUrl.shortened = baseUrl <> "/" <> short
          , ShortenedUrl.url = createUrl
          , ShortenedUrl.visits = 0
          }
  case (mbByShort, mbByLong) of
    -- Short NOT taken, create even if there's a long url already
    (Nothing, _) -> do
      void $ lift $ insertUrl $ shortened{ShortenedUrl.expires = fromMaybe False createExpires}
      lift $ incLinksCreated $ fromMaybe CreatedOnOther createCreatedOn
      pure shortened

    -- Short taken, don't create
    (Just byShort, Nothing)
      | ShortenedUrl.url byShort == createUrl ->
          pure shortened
    (Just byShort, Just byLong)
      | ShortenedUrl.url byShort == createUrl && ShortenedUrl.short byLong == short ->
          pure shortened
    _ -> throwError $ Err400 $ Error "Custom short already taken"

createWithRandomShort :: Text -> Maybe CreatedOn -> Maybe Bool -> ExceptT CreateError App ShortenedUrl
createWithRandomShort createUrl createCreatedOn createExpires = do
  mbShortened <- lift $ findUrlByLong createUrl
  baseUrl <- asks configBaseUrl
  case mbShortened of
    Nothing -> do
      seed <- maybe (throwError $ Err500 $ Error "Something went wrong") pure =<< (lift nextShortRefCounter)
      short <- lift $ asks (decodeUtf8 . flip Hashids.encode seed . configHashidsCtx)
      let shortened =
            ShortenedUrl
              { ShortenedUrl.expires = fromMaybe False createExpires
              , ShortenedUrl.short = short
              , ShortenedUrl.shortened = baseUrl <> "/" <> short
              , ShortenedUrl.url = createUrl
              , ShortenedUrl.visits = 0
              }
      void $ lift $ insertUrl shortened
      lift $ incLinksCreated $ fromMaybe CreatedOnOther createCreatedOn
      pure shortened
    Just existing -> pure existing
