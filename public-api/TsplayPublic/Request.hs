{-# LANGUAGE OverloadedStrings #-}

module TsplayPublic.Request
  ( -- * Parameters
    Style (..)
  , pathVariable
  , requiredQueryParameter
  , requiredQueryParameters
  , optionalQueryParameter
  , optionalQueryParameters
  , requiredHeader
  , optionalHeader

    -- * Request body
  , parseRequestBody
  , jsonBodyParser
  , formBodyParser
  )
where

import Data.Aeson (FromJSON, parseJSON)
import qualified Data.Aeson.Parser
import qualified Data.Aeson.Types
import Data.Attoparsec.ByteString (eitherResult, parseWith)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Media
import Network.HTTP.Types (HeaderName, hContentType)
import qualified Network.Wai as Wai
import System.IO.Unsafe (unsafeInterleaveIO)
import Web.FormUrlEncoded
  ( FromForm
  , parseAll
  , urlDecodeAsForm
  , urlDecodeForm
  )
import Web.HttpApiData
  ( FromHttpApiData
  , parseHeader
  , parseQueryParam
  , parseUrlPiece
  , parseUrlPieces
  )

pathVariable ::
  FromHttpApiData a =>
  -- | Path variable value
  Text ->
  (a -> Wai.Application) ->
  Wai.Application
pathVariable value withVariable = \request respond ->
  case parseUrlPiece value of
    Left _err ->
      respond (Wai.responseBuilder (toEnum 400) [] mempty)
    Right x ->
      withVariable x request respond
{-# INLINEABLE pathVariable #-}

data Style
  = FormStyle
  | CommaDelimitedStyle
  | SpaceDelimitedStyle
  | PipeDelimitedStyle

newtype CommaDelimitedValue a = CommaDelimitedValue {unCommaDelimitedValue :: [a]}

instance FromHttpApiData a => FromHttpApiData (CommaDelimitedValue a) where
  parseUrlPiece input = do
    xs <- parseUrlPieces (Text.splitOn "," input)
    pure (CommaDelimitedValue xs)

newtype SpaceDelimitedValue a = SpaceDelimitedValue {unSpaceDelimitedValue :: [a]}

instance FromHttpApiData a => FromHttpApiData (SpaceDelimitedValue a) where
  parseUrlPiece input = do
    xs <- parseUrlPieces (Text.splitOn " " input)
    pure (SpaceDelimitedValue xs)

newtype PipeDelimitedValue a = PipeDelimitedValue {unPipeDelimitedValue :: [a]}

instance FromHttpApiData a => FromHttpApiData (PipeDelimitedValue a) where
  parseUrlPiece input = do
    xs <- parseUrlPieces (Text.splitOn "|" input)
    pure (PipeDelimitedValue xs)

requiredQueryParameters ::
  FromHttpApiData a =>
  Style ->
  ByteString ->
  (NonEmpty.NonEmpty a -> Wai.Application) ->
  Wai.Application
requiredQueryParameters style name withParam =
  case style of
    FormStyle -> \request respond ->
      case urlDecodeForm (LBS.fromStrict (ByteString.drop 1 (Wai.rawQueryString request))) of
        Left error ->
          respond (Wai.responseBuilder (toEnum 400) [] mempty)
        Right form ->
          case parseAll (Text.decodeUtf8 name) form of
            Left _ ->
              respond (Wai.responseBuilder (toEnum 400) [] mempty)
            Right [] ->
              respond (Wai.responseBuilder (toEnum 400) [] mempty)
            Right (x : xs) ->
              withParam (x NonEmpty.:| xs) request respond
    SpaceDelimitedStyle ->
      requiredQueryParameter
        name
        ( \xs -> \request respond ->
            case NonEmpty.nonEmpty (unSpaceDelimitedValue xs) of
              Nothing ->
                respond (Wai.responseBuilder (toEnum 400) [] mempty)
              Just xs ->
                withParam xs request respond
        )
    PipeDelimitedStyle ->
      requiredQueryParameter
        name
        ( \xs -> \request respond ->
            case NonEmpty.nonEmpty (unPipeDelimitedValue xs) of
              Nothing ->
                respond (Wai.responseBuilder (toEnum 400) [] mempty)
              Just xs ->
                withParam xs request respond
        )
    CommaDelimitedStyle ->
      requiredQueryParameter
        name
        ( \xs -> \request respond ->
            case NonEmpty.nonEmpty (unCommaDelimitedValue xs) of
              Nothing ->
                respond (Wai.responseBuilder (toEnum 400) [] mempty)
              Just xs ->
                withParam xs request respond
        )

optionalQueryParameters ::
  FromHttpApiData a =>
  Style ->
  ByteString ->
  (Maybe (NonEmpty.NonEmpty a) -> Wai.Application) ->
  Wai.Application
optionalQueryParameters style name withParam =
  case style of
    FormStyle -> \request respond ->
      case urlDecodeForm (LBS.fromStrict (ByteString.drop 1 (Wai.rawQueryString request))) of
        Left error ->
          respond (Wai.responseBuilder (toEnum 400) [] mempty)
        Right form ->
          case parseAll (Text.decodeUtf8 name) form of
            Left _ ->
              respond (Wai.responseBuilder (toEnum 400) [] mempty)
            Right [] ->
              withParam Nothing request respond
            Right (x : xs) ->
              withParam (Just (x NonEmpty.:| xs)) request respond
    SpaceDelimitedStyle ->
      optionalQueryParameter
        name
        False
        ( \xs ->
            withParam (xs >>= NonEmpty.nonEmpty . unSpaceDelimitedValue)
        )
    PipeDelimitedStyle ->
      optionalQueryParameter
        name
        False
        ( \xs ->
            withParam (xs >>= NonEmpty.nonEmpty . unPipeDelimitedValue)
        )
    CommaDelimitedStyle ->
      optionalQueryParameter
        name
        False
        ( \xs ->
            withParam (xs >>= NonEmpty.nonEmpty . unCommaDelimitedValue)
        )

requiredQueryParameter ::
  FromHttpApiData a =>
  ByteString ->
  (a -> Wai.Application) ->
  Wai.Application
requiredQueryParameter name withParam = \request respond ->
  case List.lookup name (Wai.queryString request) of
    Nothing ->
      respond (Wai.responseBuilder (toEnum 400) [] mempty)
    Just Nothing ->
      respond (Wai.responseBuilder (toEnum 400) [] mempty)
    Just (Just value) ->
      case parseQueryParam (Text.decodeUtf8 value) of
        Left _err ->
          respond (Wai.responseBuilder (toEnum 400) [] mempty)
        Right x ->
          withParam x request respond
{-# INLINEABLE requiredQueryParameter #-}

optionalQueryParameter ::
  FromHttpApiData a =>
  ByteString ->
  -- | Allow empty, e.g. "x="
  Bool ->
  (Maybe a -> Wai.Application) ->
  Wai.Application
optionalQueryParameter name allowEmpty withParam = \request respond ->
  case List.lookup name (Wai.queryString request) of
    Nothing ->
      withParam Nothing request respond
    Just Nothing
      | allowEmpty ->
          withParam Nothing request respond
      | otherwise ->
          respond (Wai.responseBuilder (toEnum 400) [] mempty)
    Just (Just value) ->
      case parseQueryParam (Text.decodeUtf8 value) of
        Left _err ->
          respond (Wai.responseBuilder (toEnum 400) [] mempty)
        Right x ->
          withParam (Just x) request respond
{-# INLINEABLE optionalQueryParameter #-}

optionalHeader ::
  FromHttpApiData a =>
  HeaderName ->
  (Maybe a -> Wai.Application) ->
  Wai.Application
optionalHeader name withHeader = \request respond ->
  case List.lookup name (Wai.requestHeaders request) of
    Nothing ->
      withHeader Nothing request respond
    Just value ->
      case parseHeader value of
        Left _err ->
          respond (Wai.responseBuilder (toEnum 400) [] mempty)
        Right x ->
          withHeader (Just x) request respond
{-# INLINEABLE optionalHeader #-}

requiredHeader ::
  FromHttpApiData a =>
  HeaderName ->
  (a -> Wai.Application) ->
  Wai.Application
requiredHeader name withHeader = \request respond ->
  case List.lookup name (Wai.requestHeaders request) of
    Nothing ->
      respond (Wai.responseBuilder (toEnum 400) [] mempty)
    Just value ->
      case parseHeader value of
        Left _err ->
          respond (Wai.responseBuilder (toEnum 400) [] mempty)
        Right x ->
          withHeader x request respond
{-# INLINEABLE requiredHeader #-}

data BodyParser a
  = BodyParser
      Network.HTTP.Media.MediaType
      ((a -> Wai.Application) -> Wai.Application)

jsonBodyParser :: FromJSON a => BodyParser a
jsonBodyParser = BodyParser "application/json" parseRequestBodyJSON
{-# INLINE jsonBodyParser #-}

formBodyParser :: FromForm a => BodyParser a
formBodyParser = BodyParser "application/xxx-form-urlencoded" parseRequestBodyForm
{-# INLINE formBodyParser #-}

parseRequestBody :: [BodyParser a] -> (a -> Wai.Application) -> Wai.Application
parseRequestBody parsers withBody = \request respond -> do
  let contentType =
        fromMaybe
          "application/octet-stream"
          (List.lookup hContentType (Wai.requestHeaders request))

      bodyParser =
        Network.HTTP.Media.mapAccept
          [(mediaType, parser) | BodyParser mediaType parser <- parsers]
          contentType

  case bodyParser of
    Just parseBody ->
      parseBody withBody request respond
    Nothing ->
      respond (Wai.responseBuilder (toEnum 415) [] mempty)
{-# INLINE parseRequestBody #-}

parseRequestBodyJSON :: FromJSON a => (a -> Wai.Application) -> Wai.Application
parseRequestBodyJSON withBody = \request respond -> do
  result <- parseWith (Wai.getRequestBodyChunk request) Data.Aeson.Parser.json' mempty
  case eitherResult result of
    Left _err ->
      respond (Wai.responseBuilder (toEnum 400) [] mempty)
    Right value ->
      case Data.Aeson.Types.parseEither Data.Aeson.parseJSON value of
        Left _err ->
          respond (Wai.responseBuilder (toEnum 400) [] mempty)
        Right body ->
          withBody body request respond
{-# INLINEABLE parseRequestBodyJSON #-}

parseRequestBodyForm :: FromForm a => (a -> Wai.Application) -> Wai.Application
parseRequestBodyForm withBody = \request respond -> do
  -- Reads the body using lazy IO. Not great but it gets us
  -- going and is pretty local.
  let getBodyBytes :: IO [ByteString]
      getBodyBytes = do
        chunk <- Wai.getRequestBodyChunk request
        case chunk of
          "" -> pure []
          _ -> do
            rest <- unsafeInterleaveIO getBodyBytes
            pure (chunk : rest)

  bytes <- getBodyBytes
  case urlDecodeAsForm (LBS.fromChunks bytes) of
    Left _err ->
      respond (Wai.responseBuilder (toEnum 400) [] mempty)
    Right form ->
      withBody form request respond
{-# INLINEABLE parseRequestBodyForm #-}
