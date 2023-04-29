{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Controller.Effects.Users where

import API.Types.User (RefreshToken, UserLoginForm, UserRegisterForm (..))
import Common.Prelude (Text)
import Controller.Prelude (ServerError)
import Crypto.JOSE (JWK)
import Effectful (Dispatch (Dynamic), DispatchOf, Effect)
import Service.Types.User (LoginError, RefreshError, RegisterError)

data UserController :: Effect where
  ControllerRegisterUser :: JWK -> UserRegisterForm -> UserController m (Either ServerError (Either RegisterError Text))
  ControllerLoginUser :: JWK -> UserLoginForm -> UserController m (Either ServerError (Either LoginError Text))
  ControllerRotateRefreshToken :: JWK -> RefreshToken -> UserController m (Either ServerError (Either RefreshError Text))

type instance DispatchOf UserController = Dynamic