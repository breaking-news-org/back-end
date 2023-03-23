module API.API where

import API.API1 qualified as API1
import API.Prelude (GenericMode (type (:-)), ToServantApi, type (:>))
import API.User qualified as User
import Common.Prelude (Generic)
import Data.Data (Proxy)

data Routes route = Routes
  { api :: route :- "api" :> "user" :> User.OldAPI
  , api1 :: route :- "api1" :> ToServantApi API1.API
  }
  deriving (Generic)

type ProxyRoutes = Proxy Routes

type API = ToServantApi Routes

-- >>> :kind! API
-- API :: *
-- = ("api"
--    :> ("user"
--        :> (ReqBody' '[Required, Strict] '[JSON] UserRegistrationForm
--            :> Verb 'POST 200 '[JSON] NoContent)))
--   :<|> ("api1"
--         :> ("news"
--             :> (("create"
--                  :> (ReqBody' '[Required, Strict] '[JSON] CreateNews
--                      :> Verb 'PUT 200 '[JSON] NoContent))
--                 :<|> ("get"
--                       :> (ReqBody' '[Required, Strict] '[JSON] UTCTime
--                           :> Verb 'GET 200 '[JSON] [GetNews])))))
