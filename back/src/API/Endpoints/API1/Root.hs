module API.Endpoints.API1.Root where

import API.Endpoints.API1.News qualified as News
import API.Endpoints.API1.User qualified as User
import API.Prelude (Generic, GenericMode (type (:-)), type (:>), NamedRoutes)
import API.Types.Client (ClientToken)
import Servant.Auth ( Auth, JWT )

data API route = API
  { user :: route :- "user" :> NamedRoutes User.API
  , news :: route :- Auth '[JWT] ClientToken :> "news" :> NamedRoutes News.API
  }
  deriving (Generic)