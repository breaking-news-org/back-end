module API.Endpoints.API1.News where

import API.Prelude (Generic, GenericMode (type (:-)), JSON, NoContent, Put, QueryParam, ReqBody, UTCTime, type (:>))
import API.Types.News (CreateNews)
import Common.Prelude (Text)
import Service.Types.News qualified as API (GetNews)

data API route = API
  { create :: route :- "create" :> ReqBody '[JSON] CreateNews :> Put '[JSON] NoContent
  , get ::
      route
        :- "get"
        :> QueryParam "createdUntil" UTCTime
        :> QueryParam "createdSince" UTCTime
        :> QueryParam "createdAt" UTCTime
        :> QueryParam "creator" Text
        :> QueryParam "category" Int
        :> QueryParam "content" Text
        :> QueryParam "block" Int
        :> QueryParam "newsId" Int
        :> Put '[JSON] [API.GetNews]
  }
  deriving (Generic)