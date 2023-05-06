{-# LANGUAGE DerivingStrategies #-}

module Controller.Types.User (
  JWKSettings (..),
  module API.Types.User,
  module Service.Types.User,
) where

import API.Prelude (NominalDiffTime)
import API.Types.User (AccessToken (..), FullToken (..), RefreshToken (..), UserLoginForm (..), UserRegisterForm (..))
import Crypto.JOSE (JWK)
import Service.Types.User (RegisterError (..), RegisteredUserError (..), RotateError (..))

data JWKSettings = JWKSettings
  { _jwkSettings_jwk :: JWK
  , _jwkSettings_jwtLifetimeSeconds :: NominalDiffTime
  }