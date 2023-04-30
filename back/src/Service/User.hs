{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Service.User where

import Control.Lens.Extras (is)
import Control.Monad.Logger.Aeson
import Data.Aeson.Text (encodeToLazyText)
import Effectful
import Effectful.TH
import Persist.Effects.User (UserRepo, repoCreateSession, repoInsertUser, repoSelectSessionById, repoSelectUser, repoSessionUpdateLastAccessTokenId)
import Service.Prelude
import Service.Types.User

data UserService :: Effect where
  ServiceRegister :: UserRegisterData -> UserService m (Either RegisterError User)
  ServiceLogin :: UserLoginData -> UserService m (Either LoginError User)
  ServiceCreateSession :: ExpiresAt -> UserId -> UserService m SessionId
  ServiceRotateRefreshToken :: ExpiresAt -> SessionId -> TokenId -> UserService m (Either RotateError (TokenId, User))

-- Service

makeEffect ''UserService

-- TODO add type instance Dynamic

runUserService :: (UserRepo :> es, Logger :> es) => Eff (UserService : es) a -> Eff es a
runUserService = interpret $ \_ -> \case
  ServiceRegister UserRegisterData{..} -> do
    user <- getUser _userRegisterData_userName _userRegisterData_password
    if is _Just user
      then pure $ Left UserExists
      else do
        newUser <-
          repoInsertUser $
            -- TODO hash usernames
            InsertUser
              { _insertUser_userName = _userRegisterData_userName
              , _insertUser_hashedPassword = hashPassword _userRegisterData_password
              , _insertUser_authorName = _userRegisterData_authorName
              , -- TODO where this role is defined?
                _insertUser_role = RoleUser
              }
        withLogger $ logDebug $ "Created a new user" :# ["user" .= newUser]
        pure $ Right newUser
  ServiceLogin UserLoginData{..} -> do
    user <- getUser _userLoginData_userName _userLoginData_password
    case user of
      Just user' -> pure $ Right user'
      _ -> pure $ Left UserDoesNotExist
  ServiceCreateSession expiresAt userId -> repoCreateSession expiresAt userId
  ServiceRotateRefreshToken expiresAt sessionId tokenId -> do
    session <- repoSelectSessionById sessionId
    case session of
      Nothing -> pure $ Left SessionDoesNotExist
      Just (session1, user) -> do
        if session1._session_lastAccessTokenId > tokenId
          then pure $ Left SessionHasNewerAccessTokenId
          else do
            repoSessionUpdateLastAccessTokenId expiresAt sessionId
            pure $ Right (tokenId + 1, user)

-- TODO expired -> remove?

getUser :: (UserRepo :> es, Logger :> es) => UserName -> Password -> Eff es (Maybe User)
getUser name password = do
  user <-
    repoSelectUser $
      SelectUser
        { _selectUser_userName = name
        , _selectUser_hashedPassword = hashPassword password
        }
  withLogger $ logDebug $ "Check user: " :# ["name" .= name, "exists" .= encodeToLazyText user]
  pure user

-- TODO generate a salt, and calculate sha512(salt <> password)
hashPassword :: Password -> HashedPassword
hashPassword (Password p) = HashedPassword $ encodeUtf8 p

--
-- runLoginError :: (UserRepo :> es, Logger :> es, Error RegisterError :> es, Error LoginError :> es) => Eff (UserService : es) a -> Eff es a
-- runLoginError