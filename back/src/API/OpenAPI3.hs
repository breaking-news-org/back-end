module API.OpenAPI3 where

import API.Prelude
import API.Root (API)
import API.Types.Client ()

spec :: OpenApi
spec = toOpenApi (Proxy :: Proxy API)

writeSpec :: FilePath -> IO ()
writeSpec f = encodeFile f spec