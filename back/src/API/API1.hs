module API.API1 where

import API.Prelude
import API.Types.News
import Common.Prelude

data API route = API1
  { news :: route :- "news" :> ToServantApi NewsEP
  }
  deriving (Generic)

data NewsEP route = NewsEP
  { create :: route :- "create" :> ReqBody '[JSON] CreateNews :> Put '[JSON] NoContent -- should place type here?
  , get :: route :- "get" :> ReqBody '[JSON] UTCTime :> Put '[JSON] [GetNews]
  }
  deriving (Generic)