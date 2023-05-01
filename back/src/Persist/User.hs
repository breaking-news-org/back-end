module Persist.User (
  runUserRepo,
)
where

import Control.Lens (Bifunctor (bimap))
import Database.Esqueleto.Experimental (Entity (Entity, entityKey), fromSqlKey, innerJoin, on, selectOne, set, toSqlKey, update, (+.), (=.), (==.), type (:&) ((:&)))
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
        { _user_authorName = _insertUser_authorName
        , _user_hashedPassword = _insertUser_hashedPassword
        , _user_userName = _insertUser_userName
        , _user_id = fromIntegral $ fromSqlKey key
        , _user_role = _insertUser_role
        }
  RepoSelectRegisteredUser SelectUser{..} -> do
    user <- selectUser _selectUser_userName
    pure (user >>= (\x -> if x._user_hashedPassword == _selectUser_hashedPassword then Just x else Nothing))
  RepoSelectUserByUserName userName -> selectUser userName
  RepoCreateSession expiresAt userId -> do
    fromIntegral . fromSqlKey
      <$> withConn
        ( insert $
            Sessions
              { sessionsLastAccessTokenId = 0
              , sessionsLastRefreshTokenExpiresAt = expiresAt
              , sessionsUserId = toSqlKey (fromIntegral userId)
              }
        )
  -- TODO repo delete session
  RepoSelectSessionById sessionId -> do
    sessionUser <- withConn $ selectOne do
      (sessions :& users) <-
        from
          $ table @Sessions `innerJoin` table @Users
          `on` (\(sessions :& users) -> sessions.userId ==. users.id)
      where_ (sessions.id ==. val (toSqlKey (fromIntegral sessionId)))
      pure (sessions, users)
    pure $ bimap sessionFromModel userFromModel <$> sessionUser
  RepoSessionUpdateLastAccessTokenId expiresAt sessionId -> do
    withConn $ update $ \p -> do
      let n = p.lastAccessTokenId
      set p [SessionsLastAccessTokenId =. n +. val 1]
      set p [SessionsLastRefreshTokenExpiresAt =. val expiresAt]
      where_ (p.id ==. val (toSqlKey (fromIntegral sessionId)))

selectUser :: (IOE :> es, SqlBackendPool :> es) => UserName -> Eff es (Maybe User)
selectUser userName =
  withConn do
    user <- selectOne do
      users <- from $ table @Users
      where_ (users.userName ==. val userName)
      pure users
    pure $ userFromModel <$> user

userToModel :: InsertUser -> Model.Users
userToModel InsertUser{..} =
  Users -- this might be more complex in production code
    { usersUserName = _insertUser_userName
    , usersPassword = _insertUser_hashedPassword
    , usersAuthorName = _insertUser_authorName
    , usersRole = _insertUser_role
    }

userFromModel :: Entity Users -> User
userFromModel Entity{entityKey, entityVal = Users{..}} =
  User
    { _user_userName = usersUserName
    , _user_hashedPassword = usersPassword
    , _user_authorName = usersAuthorName
    , _user_id = fromIntegral $ fromSqlKey entityKey
    , _user_role = usersRole
    }

sessionFromModel :: Entity Sessions -> Session
sessionFromModel Entity{entityKey, entityVal = Sessions{..}} =
  Session
    { _session_lastAccessTokenId = sessionsLastAccessTokenId
    , _session_lastRefreshTokenExpiresAt = sessionsLastRefreshTokenExpiresAt
    , _session_id = fromIntegral $ fromSqlKey entityKey
    }