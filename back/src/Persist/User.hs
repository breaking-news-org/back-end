module Persist.User (
  runUserRepo,
)
where

import Common.Prelude ((<&>))
import Common.Types.User
import Control.Lens (Bifunctor (bimap))
import Database.Esqueleto.Experimental (Entity (Entity, entityKey), PersistUniqueWrite (putMany), delete, fromSqlKey, innerJoin, notIn, on, selectOne, set, update, valList, (+.), (=.), (==.), type (:&) ((:&)))
import External.Passwords (checkPassword)
import Persist.Effects.User (UserRepo (..))
import Persist.Model
import Persist.Prelude

runUserRepo :: (IOE :> es, SqlBackendPool :> es) => Eff (UserRepo : es) a -> Eff es a
runUserRepo = interpret $ \_ -> \case
  RepoInsertUser user@InsertUser{..} -> do
    key <- withConn $ insert $ userToModel user
    pure $ DBUser{_id = fromIntegral $ fromSqlKey key, ..}
  RepoSelectRegisteredUser SelectUser{..} -> do
    user <- selectUser _userName
    pure $
      case user of
        Nothing -> Left UserDoesNotExist
        Just x ->
          if checkPassword _password x._hashedPassword
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
                        { _authorName = "admin"
                        , _hashedPassword = password
                        , _role = RoleAdmin
                        , _userName = name
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
    { usersName = _userName
    , usersPassword = _hashedPassword
    , usersAuthorName = _authorName
    , usersRole = _role
    }

userFromModel :: Entity Users -> DBUser
userFromModel Entity{entityKey, entityVal = Users{..}} =
  DBUser
    { _userName = usersName
    , _hashedPassword = usersPassword
    , _authorName = usersAuthorName
    , _id = fromIntegral $ fromSqlKey entityKey
    , _role = usersRole
    }

sessionFromModel :: Entity Sessions -> Session
sessionFromModel Entity{entityKey, entityVal = Sessions{..}} =
  Session
    { _tokenId = sessionsTokenId
    , _tokenExpiresAt = sessionsTokenExpiresAt
    , _id = fromIntegral $ fromSqlKey entityKey
    }