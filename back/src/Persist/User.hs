module Persist.User (
  runUserRepo,
)
where

import Control.Lens qualified as Lens
import Control.Monad (void)
import Database.Esqueleto.Experimental (selectOne, (==.), Entity (Entity, entityKey), fromSqlKey)
import Persist.Effects.User (UserRepo (..))
import Persist.Model (EntityField (..), Users(..))
import Persist.Model qualified as Model
import Persist.Prelude
import Persist.Types.User

runUserRepo :: (IOE :> es, SqlBackendPool :> es) => Eff (UserRepo : es) a -> Eff es a
runUserRepo = interpret $ \_ -> \case
  RepoInsertUser user -> void $ withConn $ insert $ userToModel user
  RepoSelectUser SelectUser{..} -> withConn do
    user <- selectOne do
      users <- from $ table @Users
      where_ (users ^. UsersUserName ==. val (_selectUser_userName Lens.^. #_UserName))
      pure users
    pure $ userFromModel <$> user

userToModel :: InsertUser -> Model.Users
userToModel InsertUser{..} =
  Users -- this might be more complex in production code
    { usersUserName = _insertUser_userName Lens.^. #_UserName
    , usersPassword = _insertUser_hashedPassword Lens.^. #_HashedPassword
    , usersAuthorName = _insertUser_authorName Lens.^. #_AuthorName
    }

userFromModel :: Entity Users -> SelectedUser
userFromModel Entity{entityKey, entityVal = Users{..}} =
  SelectedUser
    { _selectedUser_userName = UserName usersUserName
    , _selectedUser_hashedPassword = HashedPassword usersPassword
    , _selectedUser_authorName = AuthorName usersAuthorName
    , _selectedUser_id = fromIntegral $ fromSqlKey entityKey
    }