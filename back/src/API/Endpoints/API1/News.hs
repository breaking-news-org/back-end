module API.Endpoints.API1.News where

import API.Prelude (Generic, GenericMode (type (:-)), Get, JSON, Post, ReqBody, type (:>), NamedRoutes)
import API.Types.Instances (Drop)
import API.Types.News (CreateNews, NewsIdHashed, QueryParams, SelectedNews)
import Servant.Record (RecordParam)
import Service.Types.News (SetIsPublished, UnavailableNews, SelectedCategories)

data API route = API
  { create :: route :- "create" :> ReqBody '[JSON] CreateNews :> Post '[JSON] NewsIdHashed
  , get :: route :- "get" :> RecordParam Drop QueryParams :> Get '[JSON] [SelectedNews]
  , publish :: route :- "set-published" :> ReqBody '[JSON] SetIsPublished :> Post '[JSON] UnavailableNews
  , categories :: route :- "categories" :> NamedRoutes CategoriesAPI
  }
  deriving (Generic)

newtype CategoriesAPI route = CategoriesAPI
  { get :: route :- "get" :> Get '[JSON] SelectedCategories
  } deriving Generic