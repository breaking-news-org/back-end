module Controller.Types.User where

import API.Prelude (NominalDiffTime)
import Crypto.JOSE (JWK)

data JWKSettings = JWKSettings
  { _jwk :: JWK
  , _jwtLifetimeSeconds :: NominalDiffTime
  }