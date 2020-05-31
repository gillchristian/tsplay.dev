{-# LANGUAGE FlexibleContexts #-}

module Database
  ( initializeDatabase,
    runDb1_,
    runDb1,
    runDbN_,
    runDbN,
    execDb1,
    execDb1_,
  )
where

import Config (Config, configPool)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Migration as PG
import Prelude

initializeDatabase :: FilePath -> PG.Connection -> IO ()
initializeDatabase dir con =
  PG.withTransaction con $
    mapM_ migrate [PG.MigrationInitialization, PG.MigrationDirectory dir]
  where
    migrate c = PG.runMigration $ PG.MigrationContext c True con

runDb1_ :: (MonadReader Config m, MonadIO m, PG.FromRow a) => PG.Query -> m (Maybe a)
runDb1_ query = do
  pool <- asks configPool
  liftIO $ Pool.withResource pool (\con -> listToMaybe <$> PG.query_ con query)

runDb1 ::
  (MonadReader Config m, MonadIO m, PG.FromRow a, PG.ToRow b) => PG.Query -> b -> m (Maybe a)
runDb1 query args = do
  pool <- asks configPool
  liftIO $ Pool.withResource pool (\con -> listToMaybe <$> PG.query con query args)

runDbN_ :: (MonadReader Config m, MonadIO m, PG.FromRow a) => PG.Query -> m [a]
runDbN_ query = do
  pool <- asks configPool
  liftIO $ Pool.withResource pool (`PG.query_` query)

runDbN :: (MonadReader Config m, MonadIO m, PG.FromRow a, PG.ToRow b) => PG.Query -> b -> m [a]
runDbN query args = do
  pool <- asks configPool
  liftIO $ Pool.withResource pool (\con -> PG.query con query args)

execDb1 :: (MonadReader Config m, MonadIO m, PG.ToRow b) => PG.Query -> b -> m Int64
execDb1 query args = do
  pool <- asks configPool
  liftIO $ Pool.withResource pool (\con -> PG.execute con query args)

execDb1_ :: (MonadReader Config m, MonadIO m) => PG.Query -> m Int64
execDb1_ query = do
  pool <- asks configPool
  liftIO $ Pool.withResource pool (`PG.execute_` query)
