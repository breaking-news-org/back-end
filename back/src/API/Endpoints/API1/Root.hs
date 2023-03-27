module API.Endpoints.API1.Root where

import API.Endpoints.API1.News qualified as News
import API.Endpoints.API1.User qualified as User
import API.Prelude

data API route = API1
  { user :: route :- "user" :> ToServantApi User.API
  , news :: route :- "news" :> ToServantApi News.API
  }
  deriving (Generic)