module Service.User where

import Control.Lens.Extras (is)
import Control.Monad (forM)
import Control.Monad.Logger.Aeson
import Data.Aeson.Text (encodeToLazyText)
import Effectful
import External.Passwords (Passwords, hashPassword)
import Persist.Effects.User
import Service.Effects.User
import Service.Prelude
import Service.Types.User
import Common.Types.User

runUserService :: (Passwords :> es, UserRepo :> es, Logger :> es) => Eff (UserService : es) a -> Eff es a
runUserService = interpret $ \_ -> \case
  ServiceRegister UserRegisterForm{..} -> do
    user <- getUserByUserName _userRegisterForm_userName
    if is _Just user
      then -- don't need to remove a session here
      -- because a user may have tried to register mistakenly
        pure $ Left UserExists
      else do
        hashedPassword <- hashPassword _userRegisterForm_password
        newUser <-
          repoInsertUser $
            -- TODO hash usernames
            InsertUser
              { _insertUser_userName = _userRegisterForm_userName
              , _insertUser_hashedPassword = hashedPassword
              , _insertUser_authorName = _userRegisterForm_authorName
              , _insertUser_role = RoleUser
              }
        withLogger $ logDebug $ "Created a new user" :# ["user" .= newUser]
        pure $ Right newUser
  ServiceLogin UserLoginForm{..} -> do
    user <- getRegisteredUser _userLoginForm_userName _userLoginForm_password
    case user of
      Right user' -> do
        repoRemoveSessionsByUserId user'._user_id
        pure $ Right user'
      Left err -> pure $ Left err
  ServiceCreateSession expiresAt userId -> repoCreateSession expiresAt userId
  ServiceRotateRefreshToken RefreshToken{..} newTokenExpiresAt -> do
    sessionUser <- repoSelectSessionById _refreshToken_sessionId
    case sessionUser of
      Nothing -> pure $ Left SessionDoesNotExist
      Just (session, user) -> do
        if session._session_tokenId > _refreshToken_id
          then pure $ Left SessionHasNewerRefreshTokenId
          else do
            repoSessionUpdateTokenId _refreshToken_sessionId newTokenExpiresAt
            pure $ Right (_refreshToken_id + 1, user)
  ServiceSetAdmins admins -> do
    admins' <- forM admins $ \Admin{..} -> (_admin_userName,) <$> hashPassword _admin_password
    repoUpdateAdmins admins'
  ServiceUnRegister sessionId -> do
    repoRemoveUser sessionId

getRegisteredUser :: ( UserRepo :> es, Logger :> es) =>UserName -> Password -> Eff es (Either RegisteredUserError DBUser)
getRegisteredUser name password = do
  user <-
    repoSelectRegisteredUser $
      SelectUser
        { _selectUser_userName = name
        , _selectUser_password = password
        }
  withLogger $ logDebug $ "Get registered user: " :# ["user" .= encodeToLazyText user]
  pure user

getUserByUserName :: (UserRepo :> es, Logger :> es) => UserName -> Eff es (Maybe DBUser)
getUserByUserName userName = do
  user <- repoSelectUserByUserName userName
  withLogger $ logDebug $ "Get registered user: " :# ["user" .= encodeToLazyText user]
  pure user
