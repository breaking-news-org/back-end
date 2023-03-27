module Persist.User (
  runUserRepo,
)
where

import Persist.Effects.User
import Persist.Model qualified as Model
import Persist.Prelude
import Service.Types.User
import Control.Monad (void)

runUserRepo ::
  (IOE :> es, SqlBackendPool :> es) =>
  Eff (UserRepo : es) a ->
  Eff es a
runUserRepo = interpret $ \_ -> \case
  CreateUser user -> void $ withConn $ insert $ userToModel user

userToModel :: User -> Model.Users
userToModel user =
  Model.Users -- this might be more complex in production code
    { Model.usersEmail = user._user_email
    , Model.usersHashedPassword = user._user_hashedPassword.unPassword
    , Model.usersNickname = user._user_nickname
    }
