module Controller.Effects.News where

import API.Types.News (CreateNews, GetNews)
import API.Types.User (AccessToken)
import Common.Prelude (Dispatch (Dynamic), DispatchOf, Effect)
import Controller.Prelude (NoContent (..), ServerError)
import Persist.Types.News (Filters)

data NewsController :: Effect where
  ControllerCreateNews :: AccessToken -> CreateNews -> NewsController m (Either ServerError NoContent)
  ControllerGetNews :: AccessToken -> Filters -> NewsController m (Either ServerError [GetNews])

type instance DispatchOf NewsController = 'Dynamic
