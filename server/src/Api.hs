module Api (app) where

import Config (App, AppT (runApp), Config (..))
import Control.Monad.Reader (asks, runReaderT)
import Network.HTTP.Types.Status qualified as Wai
import Network.Wai qualified as Wai
import Short.Api (createHandler, listAllHandler, visitHandler)
import Short.Persistence
import TsplayPublic.Api as TsplayPublic
import Prelude

import TsplayPublic.Response.HealthCheck (HealthCheckResponse (..))
import TsplayPublic.Response.Stats (StatsResponse (..))
import TsplayPublic.Response.VisitTsplay (VisitTsplayResponse (..))

api :: TsplayPublic.Api App
api =
  TsplayPublic.Api
    { healthCheck = pure HealthCheckResponse200
    , visitTsplay = visitTsplayHandler
    , stats = StatsResponse200 <$> urlsStats -- TODO: handle error
    , -- Shorts
      createShort = createHandler
    , visitShortened = visitHandler
    , listAllShorts = listAllHandler
    }

notFound :: Wai.Application
notFound _req respond = respond $ Wai.responseLBS Wai.status404 [] mempty

run :: Config -> Wai.Request -> App a -> IO a
run cfg _ appt = runReaderT (runApp appt) cfg

app :: Config -> Wai.Application
app cfg = TsplayPublic.application (run cfg) api notFound

visitTsplayHandler :: App VisitTsplayResponse
visitTsplayHandler = do
  client <- asks configClientUrl
  pure $ VisitTsplayResponse302 client
