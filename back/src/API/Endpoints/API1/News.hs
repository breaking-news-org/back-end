module API.Endpoints.API1.News where

import API.Prelude (Generic, GenericMode (type (:-)), Get, JSON, NamedRoutes, Post, ReqBody, type (:>))
import API.Types.QueryParam (Drop)
import Common.Types.News
import Controller.Types.News
import Servant.Record (RecordParam)

data API route = API
  { create :: route :- "create" :> ReqBody '[JSON] CreateNews :> Post '[JSON] (Either InsertNewsError NewsItem)
  , get :: route :- "get" :> RecordParam Drop NewsFilters :> Get '[JSON] [NewsItem]
  , setIsPublished :: route :- "set-is-published" :> ReqBody '[JSON] SetIsPublished :> Post '[JSON] UnavailableNews
  , categories :: route :- "categories" :> NamedRoutes CategoriesAPI
  }
  deriving (Generic)

data QueryRecord = QueryRecord {_queryRecord_include :: [Int], _queryRecord_exclude :: [Int]} 
  deriving (Generic) 

newtype CategoriesAPI route = CategoriesAPI
  { get :: route :- "get" :> RecordParam Drop CategoryFilters :> Get '[JSON] SelectedCategories
  }
  deriving (Generic)