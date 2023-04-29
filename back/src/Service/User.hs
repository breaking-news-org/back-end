{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Service.User where

import Control.Monad.Logger.Aeson
import Data.Aeson.Text (encodeToLazyText)
import Data.Maybe (isJust)
import Effectful
import Effectful.Error.Static (Error, throwError)
import Effectful.TH
import Persist.Effects.User (UserRepo, repoInsertUser, repoSelectUser)
import Persist.Types.User (HashedPassword (..))
import Service.Prelude
import Service.Types.User

data UserService :: Effect where
  ServiceRegister :: UserRegisterData -> UserService m ()
  ServiceLogin :: UserLoginData -> UserService m AuthorName

makeEffect ''UserService

-- TODO add type instance Dynamic

runUserService :: (UserRepo :> es, Logger :> es, Error RegisterError :> es, Error LoginError :> es) => Eff (UserService : es) a -> Eff es a
runUserService = interpret $ \_ -> \case
  ServiceRegister UserRegisterData{..} -> do
    existsUser <- checkExistsUser _userRegisterData_userName _userRegisterData_password
    if existsUser
      then do
        repoInsertUser $
          -- TODO hash user names
          InsertUser
            { _insertUser_userName = _userRegisterData_userName
            , _insertUser_hashedPassword = hashPassword _userRegisterData_password
            , _insertUser_authorName = _userRegisterData_authorName
            }
        withLogger $ logDebug "Created a new user"
      else throwError UserExists
  ServiceLogin UserLoginData{..} -> do
    user <- getUser _userLoginData_userName _userLoginData_password
    case user of
      Just user' -> pure $ user' ^. #_selectedUser_authorName
      _ -> throwError UserDoesNotExist

checkExistsUser :: (UserRepo :> es, Logger :> es) => UserName -> Password -> Eff es Bool
checkExistsUser name password = isJust <$> getUser name password

getUser :: (UserRepo :> es, Logger :> es) => UserName -> Password -> Eff es (Maybe SelectedUser)
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