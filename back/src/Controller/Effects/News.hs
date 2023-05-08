module Controller.Effects.News where

import API.Types.News
import Common.Prelude (Dispatch (Dynamic), DispatchOf, Effect)
import Controller.Prelude (ServerError)

data NewsController :: Effect where
  ControllerCreateNews :: AccessToken -> CreateNews -> NewsController m (Either ServerError (Either InsertNewsError NewsItem))
  ControllerSelectNews :: AccessToken -> NewsFilters -> NewsController m (Either ServerError [NewsItem])
  ControllerSetIsPublishedNews :: AccessToken -> SetIsPublished -> NewsController m (Either ServerError UnavailableNews)
  ControllerGetCategories :: CategoryFilters -> NewsController m (Either ServerError SelectedCategories)

type instance DispatchOf NewsController = 'Dynamic
