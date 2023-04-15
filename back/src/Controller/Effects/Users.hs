{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Controller.Effects.Users where

import API.Types.User (UserRegistrationForm (..))
import Common.Prelude (Text)
import Controller.Prelude (ServerError)
import Crypto.JOSE (JWK)
import Effectful (Dispatch (Dynamic ), DispatchOf, Effect)

data UserController :: Effect where
  RegisterController :: JWK -> UserRegistrationForm -> UserController m (Either ServerError Text)

type instance DispatchOf UserController = Dynamic
