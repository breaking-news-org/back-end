module Demo.Backend.Persist.User (
  runUserRepo,
)
where

import Control.Monad (void)
import Control.Monad.Logger.Aeson (logDebug, logInfo)
import Data.String (fromString)
import Demo.Backend.External.Logger (Logger, withLogger)
import Demo.Backend.Persist.Model qualified as Model
import Demo.Backend.Persist.Prelude
import Demo.Backend.Service.User

runUserRepo ::
  (IOE :> es, SqlBackendPool :> es, Logger :> es) =>
  Eff (UserRepo : es) a ->
  Eff es a
runUserRepo = interpret $ \_ -> \case
  CreateUser user -> void $ withConn $ insert $ userToModel user

-- do
-- userID <- withConn $ insert $ userToModel user
-- withConn $ get userID >>= (liftIO . print)

-- withConn $ get

userToModel :: User -> Model.Users
userToModel user =
  Model.Users -- this might be more complex in production code
    { Model.usersEmail = _user_email user
    , Model.usersHashedPassword = _user_hashedPassword user
    , Model.usersNickname = _user_nickname user
    }
