module TsplayPublic.Response
  ( ToResponse (..)

    -- * NDJSON support
  , NDJSON
  , responseNDJSON
  )
where

import qualified Data.Aeson
import qualified Data.Aeson.Encoding
import qualified Data.ByteString.Builder
import qualified Network.HTTP.Types
import qualified Network.Wai

type NDJSON element = ((element -> IO ()) -> IO () -> IO ())

responseNDJSON :: Data.Aeson.ToJSON element => Network.HTTP.Types.Status -> Network.HTTP.Types.ResponseHeaders -> NDJSON element -> Network.Wai.Response
responseNDJSON status responseHeaders stream =
  Network.Wai.responseStream status responseHeaders $ \emit flush ->
    stream
      ( \element ->
          emit
            ( Data.Aeson.Encoding.fromEncoding (Data.Aeson.toEncoding element)
                <> Data.ByteString.Builder.char7 '\n'
            )
      )
      flush

class ToResponse a where
  toResponse :: a -> Network.Wai.Response
