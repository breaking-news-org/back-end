{-# LANGUAGE TemplateHaskell #-}

module Demo.Backend.External.SqlDatabase (runSqlBackendPool, SqlBackendPool, withConn) where

import Control.Lens (to, (^.))
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Trans.Class (lift)
import Data.Pool (Pool, withResource)
import Data.Text.Encoding (encodeUtf8)
import Database.Esqueleto.Experimental (SqlBackend, runMigration)
import Database.Persist.Postgresql (withPostgresqlPool)
import Demo.Backend.Config (mkConnStr)
import Demo.Backend.Config qualified as Config
import Demo.Backend.External.Logger (Logger, withLogger)
import Demo.Backend.Persist.Model (migrateAll)
import Effectful (Eff, Effect, IOE, Limit (Unlimited), MonadIO (liftIO), Persistence (Ephemeral), UnliftStrategy (ConcUnlift), withEffToIO, withUnliftStrategy, type (:>))
import Effectful.Dispatch.Dynamic (localSeqUnliftIO, reinterpret)
import Effectful.Reader.Static (Reader, ask, runReader)
import Effectful.TH (makeEffect)

data SqlBackendPool :: Effect where
  WithConn :: ReaderT SqlBackend m a -> SqlBackendPool m a

makeEffect ''SqlBackendPool

runSqlBackendPool ::
  (IOE :> es, Config.Loader :> es, Logger :> es) =>
  Eff (SqlBackendPool : es) a ->
  Eff es a
runSqlBackendPool = reinterpret initSqlBackendPool $ \env -> \case
  WithConn action -> do
    pool <- ask
    localSeqUnliftIO env $ \unlift -> do
      withResource pool $ unlift . runReaderT action

initSqlBackendPool ::
  (IOE :> es, Config.Loader :> es, Logger :> es) =>
  Eff (Reader (Pool SqlBackend) : es) a ->
  Eff es a
initSqlBackendPool action = do
  conf <- Config.getConfig Config._app_db
  withLogger
    $ withPostgresqlPool
      (conf ^. to mkConnStr . to encodeUtf8)
      (conf ^. Config.db_numConns)
    $ \pool -> do
      lift $ withUnliftStrategy (ConcUnlift Ephemeral Unlimited) $ withEffToIO $ \unlift -> do
        liftIO $ withResource pool $ runReaderT (runMigration migrateAll)
        unlift $ runReader pool action
