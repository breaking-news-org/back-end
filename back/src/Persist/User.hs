module Persist.User (
  runUserRepo,
)
where

import Control.Lens qualified as Lens
import Database.Esqueleto.Experimental (Entity (Entity, entityKey), fromSqlKey, selectOne, (==.))
import Persist.Effects.User (UserRepo (..))
import Persist.Model (EntityField (..), Sessions (..), Users (..))
import Persist.Model qualified as Model
import Persist.Prelude
import Persist.Types.User

runUserRepo :: (IOE :> es, SqlBackendPool :> es) => Eff (UserRepo : es) a -> Eff es a
runUserRepo = interpret $ \_ -> \case
  RepoInsertUser user@InsertUser{..} -> do
    key <- withConn $ insert $ userToModel user
    pure $
      User
        { _dbUser_authorName = _insertUser_authorName
        , _dbUser_hashedPassword = _insertUser_hashedPassword
        , _dbUser_userName = _insertUser_userName
        , _dbUser_id = fromIntegral $ fromSqlKey key
        }
  RepoSelectUser SelectUser{..} -> withConn do
    user <- selectOne do
      users <- from $ table @Users
      where_ (users ^. UsersUserName ==. val _selectUser_userName)
      pure users
    pure $ userFromModel <$> user
  RepoCreateSession expiresAt -> do
    fromIntegral . fromSqlKey <$> withConn (insert $ Sessions{sessionsLastAccessTokenId = 0, sessionsLastAccessTokenExpiresAt = expiresAt})

userToModel :: InsertUser -> Model.Users
userToModel InsertUser{..} =
  Users -- this might be more complex in production code
    { usersUserName = _insertUser_userName
    , usersPassword = _insertUser_hashedPassword Lens.^. #_HashedPassword
    , usersAuthorName = _insertUser_authorName
    }

userFromModel :: Entity Users -> User
userFromModel Entity{entityKey, entityVal = Users{..}} =
  User
    { _dbUser_userName = usersUserName
    , _dbUser_hashedPassword = HashedPassword usersPassword
    , _dbUser_authorName = usersAuthorName
    , _dbUser_id = fromIntegral $ fromSqlKey entityKey
    }