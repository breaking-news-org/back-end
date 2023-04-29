module API.Endpoints.API1.News where

import API.Prelude (Generic, GenericMode (type (:-)), Get, JSON, NoContent, Post, ReqBody, type (:>))
import API.Types.Instances (Drop)
import API.Types.News (CreateNews, QueryParams)
import Servant.Record (RecordParam)
import Service.Types.News qualified as API (GetNews)

data API route = API
  { create :: route :- "create" :> ReqBody '[JSON] CreateNews :> Post '[JSON] NoContent
  , get :: route :- "get" :> RecordParam Drop QueryParams :> Get '[JSON] [API.GetNews]
  -- , my :: route
  }
  deriving (Generic)