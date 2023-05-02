{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Service.User where

import Control.Lens.Extras (is)
import Control.Monad.Logger.Aeson
import Data.Aeson.Text (encodeToLazyText)
import Effectful
import Effectful.TH
import Persist.Effects.User (UserRepo, repoCreateSession, repoInsertUser, repoSelectRegisteredUser, repoSelectSessionById, repoSelectUserByUserName, repoSessionUpdateLastAccessTokenId)
import Service.Prelude
import Service.Types.User

data UserService :: Effect where
  ServiceRegister :: UserRegisterForm -> UserService m (Either RegisterError DBUser)
  ServiceLogin :: UserLoginForm -> UserService m (Either LoginError DBUser)
  ServiceCreateSession :: ExpiresAt -> UserId -> UserService m SessionId
  ServiceRotateRefreshToken :: ExpiresAt -> SessionId -> TokenId -> UserService m (Either RotateError (TokenId, DBUser))

makeEffect ''UserService

runUserService :: (UserRepo :> es, Logger :> es) => Eff (UserService : es) a -> Eff es a
runUserService = interpret $ \_ -> \case
  ServiceRegister UserRegisterForm{..} -> do
    user <- getUserByUserName _userRegisterForm_userName
    if is _Just user
      then pure $ Left UserExists
      else do
        newUser <-
          repoInsertUser $
            -- TODO hash usernames
            InsertUser
              { _insertUser_userName = _userRegisterForm_userName
              , _insertUser_hashedPassword = hashPassword _userRegisterForm_password
              , _insertUser_authorName = _userRegisterForm_authorName
              , -- TODO where this role is defined?
                _insertUser_role = RoleUser
              }
        withLogger $ logDebug $ "Created a new user" :# ["user" .= newUser]
        pure $ Right newUser
  ServiceLogin UserLoginForm{..} -> do
    user <- getRegisteredUser _userLoginForm_userName _userLoginForm_password
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
          then pure $ Left SessionHasNewerRefreshTokenId
          else do
            repoSessionUpdateLastAccessTokenId expiresAt sessionId
            pure $ Right (tokenId + 1, user)

getRegisteredUser :: (UserRepo :> es, Logger :> es) => UserName -> Password -> Eff es (Maybe DBUser)
getRegisteredUser name password = do
  user <-
    repoSelectRegisteredUser $
      SelectUser
        { _selectUser_userName = name
        , _selectUser_hashedPassword = hashPassword password
        }
  withLogger $ logDebug $ "Get registered user: " :# ["user" .= encodeToLazyText user]
  pure user

getUserByUserName :: (UserRepo :> es, Logger :> es) => UserName -> Eff es (Maybe DBUser)
getUserByUserName userName = do
  user <- repoSelectUserByUserName userName
  withLogger $ logDebug $ "Get registered user: " :# ["user" .= encodeToLazyText user]
  pure user

-- TODO generate a salt, and calculate sha512(salt <> password)
hashPassword :: Password -> HashedPassword
hashPassword (Password p) = HashedPassword $ encodeUtf8 p
