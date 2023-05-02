module API.Root where

import API.Endpoints.API1.Root qualified as API1
import API.Prelude (GenericMode (type (:-)), NamedRoutes, type (:>))
import Common.Prelude (Generic)
import Data.Data (Proxy)

newtype Routes route = Routes
  { api1 :: route :- "api1" :> NamedRoutes API1.API
  }
  deriving (Generic)

type ProxyRoutes = Proxy Routes

type API = NamedRoutes Routes
