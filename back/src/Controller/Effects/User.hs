module Controller.Effects.User where

import Controller.Prelude (ExceptT (..), ServerError)
import Controller.Types.User (FullToken, JWKSettings, LoginError, RefreshToken, RegisterError, RotateError, UserLoginForm, UserRegisterForm)
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (send)

data UserController :: Effect where
  ControllerRegisterUser :: JWKSettings -> UserRegisterForm -> UserController m (Either ServerError (Either RegisterError FullToken))
  ControllerLoginUser :: JWKSettings -> UserLoginForm -> UserController m (Either ServerError (Either LoginError FullToken))
  ControllerRotateRefreshToken :: JWKSettings -> RefreshToken -> UserController m (Either ServerError (Either RotateError FullToken))

type instance DispatchOf UserController = 'Dynamic
