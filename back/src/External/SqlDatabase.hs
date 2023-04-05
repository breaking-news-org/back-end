module External.SqlDatabase (runSqlBackendPool, SqlBackendPool, withConn) where

import Control.Lens (to, (^.))
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Trans.Class (lift)
import Data.Pool (Pool, withResource)
import Data.Text.Encoding (encodeUtf8)
import Database.Esqueleto.Experimental (SqlBackend, runMigration)
import Database.Persist.Postgresql (withPostgresqlPool)
import Effectful (Eff, Effect, IOE, Limit (Unlimited), MonadIO (liftIO), Persistence (Ephemeral), UnliftStrategy (ConcUnlift), withEffToIO, withUnliftStrategy, type (:>))
import Effectful.Dispatch.Dynamic (localSeqUnliftIO, reinterpret)
import Effectful.Reader.Static (Reader, ask, runReader)
import Effectful.TH (makeEffect)
import External.Logger (Logger, withLogger)
import Persist.Model (migrateAll)
import Server.Config (App (_app_db), Loader, db_numConns, getConfig, mkConnStr)

data SqlBackendPool :: Effect where
  WithConn :: ReaderT SqlBackend m a -> SqlBackendPool m a

makeEffect ''SqlBackendPool

runSqlBackendPool ::
  (IOE :> es, Loader App :> es, Logger :> es) =>
  Eff (SqlBackendPool : es) a ->
  Eff es a
runSqlBackendPool = reinterpret initSqlBackendPool $ \env -> \case
  WithConn action -> do
    pool <- ask
    localSeqUnliftIO env $ \unlift -> do
      withResource pool $ unlift . runReaderT action

initSqlBackendPool ::
  (IOE :> es, Loader App :> es, Logger :> es) =>
  Eff (Reader (Pool SqlBackend) : es) a ->
  Eff es a
initSqlBackendPool action = do
  conf <- getConfig _app_db
  withLogger
    $ withPostgresqlPool
      (conf ^. to mkConnStr . to encodeUtf8)
      (conf ^. db_numConns)
    $ \pool -> do
      lift $ withUnliftStrategy (ConcUnlift Ephemeral Unlimited) $ withEffToIO $ \unlift -> do
        liftIO $ withResource pool $ runReaderT (runMigration migrateAll)
        unlift $ runReader pool action
