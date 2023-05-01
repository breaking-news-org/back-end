{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Controller.Effects.Users where

import API.Types.User (FullToken, RefreshToken, UserLoginForm, UserRegisterForm (..))
import Controller.Prelude (ServerError)
import Controller.Types.User (JWKSettings)
import Effectful (Dispatch (Dynamic), DispatchOf, Effect)
import Service.Types.User (LoginError, RegisterError, RotateError)

data UserController :: Effect where
  ControllerRegisterUser :: JWKSettings -> UserRegisterForm -> UserController m (Either ServerError (Either RegisterError FullToken))
  ControllerLoginUser :: JWKSettings -> UserLoginForm -> UserController m (Either ServerError (Either LoginError FullToken))
  ControllerRotateRefreshToken :: JWKSettings -> RefreshToken -> UserController m (Either ServerError (Either RotateError FullToken))

type instance DispatchOf UserController = Dynamic