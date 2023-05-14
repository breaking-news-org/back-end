module Controller.Types.User where

import API.Prelude (NominalDiffTime)
import Crypto.JOSE (JWK)

data JWKSettings = JWKSettings
  { _jwkSettings_jwk :: JWK
  , _jwkSettings_jwtLifetimeSeconds :: NominalDiffTime
  }