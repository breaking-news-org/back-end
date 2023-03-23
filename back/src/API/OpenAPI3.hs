module API.OpenAPI3 where

import API.API (API)
import API.Prelude
import Common.Prelude (ByteString)
import Servant.OpenApi (HasOpenApi (toOpenApi))

s :: ByteString
s = encode $ toOpenApi (Proxy :: Proxy API)
