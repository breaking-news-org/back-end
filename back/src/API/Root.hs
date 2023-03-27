module API.Root where

import API.Endpoints.API1.Root qualified as API1
import API.Prelude (GenericMode (type (:-)), ToServantApi, type (:>))
import Common.Prelude (Generic)
import Data.Data (Proxy)

data Routes route = Routes
  { api1 :: route :- "api1" :> ToServantApi API1.API
  }
  deriving (Generic)

type ProxyRoutes = Proxy Routes

type API = ToServantApi Routes

-- >>> :kind! API
-- API :: *
-- = "api1"
--   :> (("user"
--        :> ("register"
--            :> (ReqBody' '[Required, Strict] '[JSON] UserRegistrationForm
--                :> Verb 'PUT 200 '[JSON] NoContent)))
--       :<|> ("news"
--             :> (("create"
--                  :> (ReqBody' '[Required, Strict] '[JSON] CreateNews
--                      :> Verb 'PUT 200 '[JSON] NoContent))
--                 :<|> ("get"
--                       :> (ReqBody' '[Required, Strict] '[JSON] (Filters Maybe)
--                           :> Verb 'PUT 200 '[JSON] [GetNews])))))
