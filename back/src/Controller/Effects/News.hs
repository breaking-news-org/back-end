module Controller.Effects.News where

import API.Types.Client (ClientToken)
import API.Types.News (CreateNews, GetNews)
import Common.Prelude (Dispatch (Dynamic), DispatchOf, Effect)
import Controller.Prelude (NoContent (..), ServerError)
import Persist.Types.News (Filters)

data NewsController :: Effect where
  ControllerCreateNews :: ClientToken -> CreateNews -> NewsController m (Either ServerError NoContent)
  ControllerGetNews :: ClientToken -> Filters Maybe -> NewsController m (Either ServerError [GetNews])

type instance DispatchOf NewsController = 'Dynamic
