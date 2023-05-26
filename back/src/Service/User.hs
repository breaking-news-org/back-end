module Service.User where

import Common.Types.User
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

runUserService :: (Passwords :> es, UserRepo :> es, Logger :> es) => Eff (UserService : es) a -> Eff es a
runUserService = interpret $ \_ -> \case
  ServiceRegister UserRegisterForm{..} -> do
    user <- getUserByUserName _userName
    if is _Just user
      then -- don't need to remove a session here
      -- because a user may have tried to register mistakenly
        pure $ Left UserExists
      else do
        _hashedPassword <- hashPassword _password
        newUser <-
          -- TODO hash usernames
          repoInsertUser $ InsertUser{_role = RoleUser, ..}
        withLogger $ logDebug $ "Created a new user" :# ["user" .= newUser]
        pure $ Right newUser
  ServiceLogin UserLoginForm{..} -> do
    user <- getRegisteredUser _userName _password
    case user of
      Right user' -> do
        repoRemoveSessionsByUserId user'._id
        pure $ Right user'
      Left err -> pure $ Left err
  ServiceCreateSession expiresAt userId -> repoCreateSession expiresAt userId
  ServiceRotateRefreshToken RefreshToken{..} newTokenExpiresAt -> do
    sessionUser <- repoSelectSessionById _sessionId
    case sessionUser of
      Nothing -> pure $ Left SessionDoesNotExist
      Just (session, user) -> do
        if session._tokenId > _id
          then pure $ Left SessionHasNewerRefreshTokenId
          else do
            repoSessionUpdateTokenId _sessionId newTokenExpiresAt
            pure $ Right (_id + 1, user)
  ServiceSetAdmins admins -> do
    admins' <- forM admins $ \Admin{..} -> (_userName,) <$> hashPassword _password
    repoUpdateAdmins admins'
  ServiceUnRegister sessionId -> do
    repoRemoveUser sessionId

getRegisteredUser :: (UserRepo :> es, Logger :> es) => UserName -> Password -> Eff es (Either RegisteredUserError DBUser)
getRegisteredUser _userName _password = do
  user <- repoSelectRegisteredUser $ SelectUser{..}
  withLogger $ logDebug $ "Get registered user: " :# ["user" .= encodeToLazyText user]
  pure user

getUserByUserName :: (UserRepo :> es, Logger :> es) => UserName -> Eff es (Maybe DBUser)
getUserByUserName userName = do
  user <- repoSelectUserByUserName userName
  withLogger $ logDebug $ "Get registered user: " :# ["user" .= encodeToLazyText user]
  pure user
