module API.OpenAPI3 where

import API.Prelude
import API.Root (API)

spec :: OpenApi
spec = toOpenApi (Proxy :: Proxy API)

writeSpec :: FilePath -> IO ()
writeSpec f = encodeFile f spec