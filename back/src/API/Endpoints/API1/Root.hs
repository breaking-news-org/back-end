module API.Endpoints.API1.Root where

import API.Endpoints.API1.News qualified as News
import API.Endpoints.API1.User qualified as User
import API.Prelude (Generic, GenericMode (type (:-)), ToServantApi, type (:>))
import API.Types.Client (ClientToken)
import Servant.Auth ( Auth, JWT )

data API route = API
  { user :: route :- "user" :> ToServantApi User.API
  , news :: route :- "news" :> Auth '[JWT] ClientToken :> ToServantApi News.API
  }
  deriving (Generic)