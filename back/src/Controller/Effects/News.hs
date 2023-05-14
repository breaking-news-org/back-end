module Controller.Effects.News where

import Common.Prelude
import Common.Types.News
import Common.Types.User
import Controller.Prelude
import Service.Types.News

data NewsController :: Effect where
  ControllerCreateNews :: AccessToken -> CreateNews -> NewsController m (Either ServerError (Either InsertNewsError NewsItem))
  ControllerSelectNews :: AccessToken -> NewsFilters -> NewsController m (Either ServerError [NewsItem])
  ControllerSetIsPublishedNews :: AccessToken -> SetIsPublished -> NewsController m (Either ServerError UnavailableNews)
  ControllerGetCategories :: CategoryFilters -> NewsController m (Either ServerError SelectedCategories)

type instance DispatchOf NewsController = 'Dynamic
