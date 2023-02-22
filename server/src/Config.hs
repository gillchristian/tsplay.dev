{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Config (
    AppT (..),
    App,
    Config (..),
    Environment (..),
    makePool,
)
where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.ByteString.Char8 qualified as BS
import Data.Pool qualified as Pool
import Data.Text (Text)
import Database.PostgreSQL.Simple qualified as PG
import Network.Wai.Handler.Warp (Port)
import System.Environment (lookupEnv)
import Web.Hashids qualified as Hashids
import Prelude

newtype AppT m a = AppT {runApp :: ReaderT Config m a}
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadReader Config
        , MonadIO
        )

type App = AppT IO

data Config = Config
    { configPool :: ConnectionPool
    , configEnv :: Environment
    , configPort :: Port
    , configHashidsCtx :: Hashids.HashidsContext
    , configBaseUrl :: Text
    , configClientUrl :: Text
    }

data Environment
    = Development
    | Test
    | Production
    deriving stock (Eq, Show, Read)

createPool :: (MonadIO m) => BS.ByteString -> Environment -> m ConnectionPool
createPool url env = liftIO $ Pool.createPool open PG.close 1 10 (envPool env)
  where
    open = PG.connectPostgreSQL url

type ConnectionPool = Pool.Pool PG.Connection

makePool :: Environment -> IO ConnectionPool
makePool Test = createPool (connStr "-test") Test
makePool Development = createPool (connStr "") Development
makePool Production = do
    pool <- runMaybeT $ do
        let keys =
                [ "host="
                , "port="
                , "user="
                , "password="
                , "dbname="
                ]
            envs =
                [ "PGHOST"
                , "PGPORT"
                , "PGUSER"
                , "PGPASS"
                , "PGDATABASE"
                ]
        envVars <- traverse (MaybeT . lookupEnv) envs
        let prodStr = BS.intercalate " " . zipWith (<>) keys $ BS.pack <$> envVars
        createPool prodStr Production
    case pool of
        Nothing -> throwIO (userError "Database Configuration not present in environment.")
        Just a -> pure a

-- | The number of pools to use for a given environment.
envPool :: Environment -> Int
envPool Test = 1
envPool Development = 1
envPool Production = 5

connStr :: BS.ByteString -> BS.ByteString
connStr sfx =
    -- TODO: this should read the port from environment
    "host=localhost dbname=development" <> sfx <> " user=development password=development port=8001"
