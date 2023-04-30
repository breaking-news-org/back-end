{-# LANGUAGE DerivingStrategies #-}

module Controller.Types.User (
  AccessToken (..),
  JWKSettings(..)
) where

import API.Types.User (AccessToken (..))

import API.Prelude (NominalDiffTime)
import Crypto.JOSE (JWK)

data JWKSettings = JWKSettings
  { _jwkSettings_jwk :: JWK
  , _jwkSettings_jwtLifetimeSeconds :: NominalDiffTime
  }