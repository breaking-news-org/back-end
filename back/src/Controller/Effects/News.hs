module Controller.Effects.News where

import API.Types.News
import API.Types.User (AccessToken (..))
import Common.Prelude (Dispatch (Dynamic), DispatchOf, Effect)
import Controller.Prelude (NoContent (..), ServerError)
import Controller.Types.News (Filters)
import Service.Types.News (UnavailableNews, SetIsPublished)

data NewsController :: Effect where
  ControllerCreateNews :: AccessToken -> CreateNews -> NewsController m (Either ServerError NoContent)
  ControllerSelectedNews :: AccessToken -> Filters -> NewsController m (Either ServerError [SelectedNews])
  ControllerSetIsPublishedNews :: AccessToken -> SetIsPublished -> NewsController m (Either ServerError UnavailableNews)

type instance DispatchOf NewsController = 'Dynamic
