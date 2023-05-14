module API.Endpoints.API1.Root where

import API.Endpoints.API1.News qualified as News
import API.Endpoints.API1.User qualified as User
import API.Prelude (Generic, GenericMode (type (:-)), NamedRoutes, type (:>))
import Common.Types.User (AccessToken)
import Servant (Raw)
import Servant.Auth (Auth, JWT)

data API route = API
  { user :: route :- "user" :> NamedRoutes User.API
  , news :: route :- "news" :> Auth '[JWT] AccessToken :> NamedRoutes News.API
  , docs :: route :- Raw
  }
  deriving (Generic)