module Controller.Effects.News where

import API.Types.Client (ClientToken)
import API.Types.News (CreateNews, GetNews)
import Common.Prelude (Dispatch (Dynamic), DispatchOf, Effect)
import Controller.Prelude (NoContent (..), ServerError)
import Persist.Types.News (Filters)

data ControllerNews :: Effect where
  ControllerCreateNews :: ClientToken -> CreateNews -> ControllerNews m (Either ServerError NoContent)
  ControllerGetNews :: ClientToken -> Filters Maybe -> ControllerNews m (Either ServerError [GetNews])

type instance DispatchOf ControllerNews = 'Dynamic
