module API.Endpoints.API1.Root where

import API.Endpoints.API1.News qualified as News
import API.Endpoints.API1.User qualified as User
import API.Prelude (Generic, GenericMode (type (:-)), NamedRoutes, type (:>))
import API.Types.User (AccessToken)
import Servant.Auth (Auth, JWT)

data API route = API
  { user :: route :- "user" :> NamedRoutes User.API
  , news :: route :- "news" :> Auth '[JWT] AccessToken :> NamedRoutes News.API
  }
  deriving (Generic)

-- rotateRefreshToken :: route :- "rotate-refresh-token" :> ReqBody '[JSON] RefreshToken :> Post '[JSON] Text