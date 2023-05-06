module Controller.Effects.User where

import Controller.Prelude (ServerError)
import Controller.Types.User (FullToken, JWKSettings)
import Effectful (Dispatch (Dynamic), DispatchOf, Effect)
import Service.Types.User

data UserController :: Effect where
  ControllerRegister :: JWKSettings -> UserRegisterForm -> UserController m (Either ServerError (Either RegisterError FullToken))
  ControllerUnRegister :: RefreshToken -> UserController m (Either ServerError ())
  ControllerLogin :: JWKSettings -> UserLoginForm -> UserController m (Either ServerError (Either RegisteredUserError FullToken))
  ControllerRotateRefreshToken :: JWKSettings -> RefreshToken -> UserController m (Either ServerError (Either RotateError FullToken))
  ControllerUpdateAdmins :: [Admin] -> UserController m ()

type instance DispatchOf UserController = 'Dynamic
