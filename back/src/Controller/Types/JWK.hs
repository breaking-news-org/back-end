{-# LANGUAGE DerivingStrategies #-}

module Controller.Types.JWK where

import API.Prelude (NominalDiffTime)
import Crypto.JOSE (JWK)

data JWKSettings = JWKSettings
  { _jwkSettings_jwk :: JWK
  , _jwkSettings_jwtLifetimeSeconds :: NominalDiffTime
  }