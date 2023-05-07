module Controller.Effects.News where

import API.Types.News
import Common.Prelude (Dispatch (Dynamic), DispatchOf, Effect)
import Controller.Prelude (ServerError)
import Controller.Types.News (Filters)
import Service.Types.News (SelectedCategories, UnavailableNews)
import Service.Types.User

data NewsController :: Effect where
  ControllerCreateNews :: AccessToken -> CreateNews -> NewsController m (Either ServerError NewsIdHashed)
  ControllerSelectedNews :: AccessToken -> Filters -> NewsController m (Either ServerError [SelectedNews])
  ControllerSetIsPublishedNews :: AccessToken -> SetIsPublished -> NewsController m (Either ServerError UnavailableNews)
  ControllerGetCategories :: NewsController m (Either ServerError SelectedCategories)

type instance DispatchOf NewsController = 'Dynamic
