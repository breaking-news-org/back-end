module API.Endpoints.API1.News where

import API.Prelude
import API.Types.News
import API.Types.News qualified as API
import Common.Prelude (Text)

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