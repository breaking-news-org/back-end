module Persist.User (
  runUserRepo,
)
where

import Common.Prelude ((<&>))
import Control.Lens (Bifunctor (bimap))
import Database.Esqueleto.Experimental (Entity (Entity, entityKey), PersistUniqueWrite (putMany), delete, fromSqlKey, innerJoin, notIn, on, selectOne, set, update, valList, (+.), (=.), (==.), type (:&) ((:&)))
import Persist.Effects.User (UserRepo (..))
import Persist.Model
import Persist.Prelude
import Persist.Types.User
import External.Passwords (checkPassword)

runUserRepo :: (IOE :> es, SqlBackendPool :> es) => Eff (UserRepo : es) a -> Eff es a
runUserRepo = interpret $ \_ -> \case
  RepoInsertUser user@InsertUser{..} -> do
    key <- withConn $ insert $ userToModel user
    pure $
      DBUser
        { _user_authorName = _insertUser_authorName
        , _user_hashedPassword = _insertUser_hashedPassword
        , _user_userName = _insertUser_userName
        , _user_id = fromIntegral $ fromSqlKey key
        , _user_role = _insertUser_role
        }
  RepoSelectRegisteredUser SelectUser{..} -> do
    user <- selectUser _selectUser_userName
    pure $
      case user of
        Nothing -> Left UserDoesNotExist
        Just x ->
          if checkPassword _selectUser_password x._user_hashedPassword
            then Right x
            else Left WrongPassword
  RepoSelectUserByUserName userName -> selectUser userName
  RepoCreateSession expiresAt userId -> do
    withConn
      ( do
          delete do
            session <- from $ table @Sessions
            where_ (session.userId ==. mkSqlKeyVal userId)
          sessionKey <-
            insert $
              Sessions
                { sessionsTokenId = 0
                , sessionsTokenExpiresAt = expiresAt
                , sessionsUserId = mkSqlKey userId
                }
          pure $ fromIntegral $ fromSqlKey sessionKey
      )
  RepoSelectSessionById sessionId -> do
    sessionUser <- withConn $ selectOne do
      (sessions :& users) <-
        from
          $ table @Sessions `innerJoin` table @Users
          `on` (\(sessions :& users) -> sessions.userId ==. users.id)
      where_ (sessions.id ==. mkSqlKeyVal sessionId)
      pure (sessions, users)
    pure $ bimap sessionFromModel userFromModel <$> sessionUser
  RepoSessionUpdateTokenId sessionId expiresAt -> do
    withConn $ update $ \p -> do
      let n = p.tokenId
      set
        p
        [ SessionsTokenId =. n +. val 1
        , SessionsTokenExpiresAt =. val expiresAt
        ]
      where_ (p.id ==. mkSqlKeyVal sessionId)
  RepoUpdateAdmins admins -> do
    let adminUsers =
          admins
            <&> ( \(name, password) ->
                    userToModel
                      InsertUser
                        { _insertUser_authorName = "admin"
                        , _insertUser_hashedPassword = password
                        , _insertUser_role = RoleAdmin
                        , _insertUser_userName = name
                        }
                )
    withConn $ do
      putMany adminUsers
      update $ \p -> do
        set p [UsersRole =. val RoleUser]
        where_ (p.role ==. val RoleAdmin &&. p.name `notIn` valList (fst <$> admins))
  RepoRemoveUser sessionId -> do
    withConn $ do
      userId <- selectOne do
        session <- from $ table @Sessions
        where_ (session.id ==. mkSqlKeyVal sessionId)
        pure session.userId
      maybe
        (pure ())
        ( \(unValue -> userId') ->
            delete do
              users <- from $ table @Users
              where_ (users.id ==. val userId')
        )
        userId
  RepoRemoveSessionsByUserId userId -> do
    withConn $ do
      delete do
        session <- from $ table @Sessions
        where_ (session.userId ==. mkSqlKeyVal userId)

selectUser :: (IOE :> es, SqlBackendPool :> es) => UserName -> Eff es (Maybe DBUser)
selectUser userName =
  withConn do
    user <- selectOne do
      users <- from $ table @Users
      where_ (users.name ==. val userName)
      pure users
    pure $ userFromModel <$> user

userToModel :: InsertUser -> Users
userToModel InsertUser{..} =
  Users -- this might be more complex in production code
    { usersName = _insertUser_userName
    , usersPassword = _insertUser_hashedPassword
    , usersAuthorName = _insertUser_authorName
    , usersRole = _insertUser_role
    }

userFromModel :: Entity Users -> DBUser
userFromModel Entity{entityKey, entityVal = Users{..}} =
  DBUser
    { _user_userName = usersName
    , _user_hashedPassword = usersPassword
    , _user_authorName = usersAuthorName
    , _user_id = fromIntegral $ fromSqlKey entityKey
    , _user_role = usersRole
    }

sessionFromModel :: Entity Sessions -> Session
sessionFromModel Entity{entityKey, entityVal = Sessions{..}} =
  Session
    { _session_tokenId = sessionsTokenId
    , _session_tokenExpiresAt = sessionsTokenExpiresAt
    , _session_id = fromIntegral $ fromSqlKey entityKey
    }