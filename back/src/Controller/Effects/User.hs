module Controller.Effects.User where

import Controller.Prelude
import Controller.Types.User
import Effectful
import Service.Types.User
import API.Types.User
import Common.Types.User

data UserController :: Effect where
  ControllerRegister :: JWKSettings -> UserRegisterForm -> UserController m (Either ServerError (Either RegisterError FullToken))
  ControllerUnRegister :: RefreshToken -> UserController m (Either ServerError ())
  ControllerLogin :: JWKSettings -> UserLoginForm -> UserController m (Either ServerError (Either RegisteredUserError FullToken))
  ControllerRotateRefreshToken :: JWKSettings -> RefreshToken -> UserController m (Either ServerError (Either RotateError FullToken))
  ControllerUpdateAdmins :: [Admin] -> UserController m (Either ServerError ())

type instance DispatchOf UserController = 'Dynamic
