module API.Endpoints.API1.News where

import API.Prelude (Generic, GenericMode (type (:-)), JSON, NoContent, Post, Put, ReqBody, type (:>))
import API.Types.News (CreateNews, QueryParams)
import Servant.Record (RecordParam)
import Service.Types.News qualified as API (GetNews)
import API.Types.Instances (Drop)

data API route = API
  { create :: route :- "create" :> ReqBody '[JSON] CreateNews :> Post '[JSON] NoContent
  , get :: route :- "get" :> RecordParam Drop QueryParams :> Put '[JSON] [API.GetNews]
  }
  deriving (Generic)