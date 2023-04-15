module API.Root where

import API.Endpoints.API1.Root qualified as API1
import API.Prelude (GenericMode (type (:-)), type (:>), NamedRoutes)
import Common.Prelude (Generic)
import Data.Data (Proxy)

data Routes route = Routes
  { api1 :: route :- "api1" :> NamedRoutes API1.API
  }
  deriving (Generic)

type ProxyRoutes = Proxy Routes

type API = NamedRoutes Routes

-- >>> :kind! API
-- API :: *
-- = "api1"
--   :> (("user"
--        :> ("authorize"
--            :> (ReqBody' '[Required, Strict] '[JSON] UserRegistrationForm
--                :> Verb 'POST 200 '[JSON] Text)))
--       :<|> (Auth '[JWT] ClientToken
--             :> ("news"
--                 :> (("create"
--                      :> (ReqBody' '[Required, Strict] '[JSON] CreateNews
--                          :> Verb 'POST 200 '[JSON] NoContent))
--                     :<|> ("get"
--                           :> (ReqBody' '[Required, Strict] '[JSON] QueryParams
--                               :> Verb 'PUT 200 '[JSON] [GetNews]))))))
