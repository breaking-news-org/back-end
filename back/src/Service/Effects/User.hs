module Service.Effects.User where

import Effectful
import Effectful.TH
import Service.Types.User
import Common.Types.User

data UserService :: Effect where
  ServiceRegister :: UserRegisterForm -> UserService m (Either RegisterError DBUser)
  ServiceUnRegister :: SessionId -> UserService m ()
  ServiceLogin :: UserLoginForm -> UserService m (Either RegisteredUserError DBUser)
  -- create a session with a refresh token that expires at a given time
  ServiceCreateSession :: ExpiresAt -> UserId -> UserService m SessionId
  ServiceRotateRefreshToken :: RefreshToken -> ExpiresAt -> UserService m (Either RotateError (TokenId, DBUser))
  ServiceSetAdmins :: [Admin] -> UserService m ()

makeEffect ''UserService
