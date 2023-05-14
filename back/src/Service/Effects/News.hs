module Service.Effects.News where

import Effectful
import Effectful.TH
import Service.Types.News
import Common.Types.User
import Common.Types.News

data ServiceNews :: Effect where
  ServiceCreateNews :: UserId -> CreateNews -> ServiceNews m (Either InsertNewsError NewsItem)
  ServiceSelectNews :: UserId -> Role -> NewsFilters -> ServiceNews m [NewsItem]
  ServiceSetIsPublished :: UserId -> Role -> SetIsPublished -> ServiceNews m UnavailableNews
  ServiceGetCategories :: CategoryFilters -> ServiceNews m SelectedCategories

makeEffect ''ServiceNews