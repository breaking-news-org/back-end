{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Service.User where

import Effectful
import Effectful.TH
import Persist.Effects.User (UserRepo, createUser)
import Service.Prelude
import Service.Types.User

data UserService :: Effect where
  ServiceRegister :: UserRegistrationData -> UserService m ()

makeEffect ''UserService

runUserService :: (UserRepo :> es, Logger :> es) => Eff (UserService : es) a -> Eff es a
runUserService = interpret $ \_ -> \case
  ServiceRegister UserRegistrationData{..} -> do
    createUser $
      User
        { _user_email = _userRegistrationData_email
        , _user_hashedPassword = Password $ hashPassword _userRegistrationData_password
        , _user_nickname = "" -- default nick name
        }
    withLogger $ logInfo "created a new user successfully"

hashPassword :: Text -> ByteString
hashPassword = encodeUtf8 -- dummy implementation. we should generate a salt, and calculate sha512(salt <> password)
