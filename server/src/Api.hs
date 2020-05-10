{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api
  ( app,
    shortApp,
  )
where

import Api.Short (ShortAPI, shortApi, shortServer)
import Config (AppT (..), Config (..))
import Control.Monad.Reader (runReaderT)
import Servant
  ( -- (:<|>) ((:<|>)),
    Proxy (Proxy),
    Server,
    serve,
  )
import Servant.Server
import Prelude

-- | This is the function we export to run our 'Short
-- a 'Config', we return a WAI 'Application' which any WAI compliant server
-- can run.
shortApp :: Config -> Application
shortApp cfg = serve shortApi (appToServer cfg)

-- | This functions tells Servant how to run the 'App' monad with our
-- 'server' function.
appToServer :: Config -> Server ShortAPI
appToServer cfg = hoistServer shortApi (convertApp cfg) shortServer

-- | This function converts our @'AppT' m@ monad into the @ExceptT ServantErr
-- m@ monad that Servant's 'enter' function needs in order to run the
-- application.
convertApp :: Config -> AppT IO a -> Handler a
convertApp cfg appt = Handler $ runReaderT (runApp appt) cfg

type AppAPI = ShortAPI

appApi :: Proxy AppAPI
appApi = Proxy

app :: Config -> Application
app cfg = serve appApi $ appToServer cfg
