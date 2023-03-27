module API.Endpoints.API1.News where

import API.Prelude
import API.Types.News

data API route = API
  { create :: route :- "create" :> ReqBody '[JSON] CreateNews :> Put '[JSON] NoContent
  , get :: route :- "get" :> ReqBody '[JSON] (Filters Maybe) :> Put '[JSON] [GetNews]
  }
  deriving (Generic)