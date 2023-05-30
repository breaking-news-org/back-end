module Service.Effects.News where

import Effectful
import Effectful.TH
import Service.Types.News
import Common.Types.User
import Common.Types.News

data ServiceNews :: Effect where
  ServiceCreateNews :: UserId -> CreateNews -> ServiceNews m (Either InsertNewsError ())
  ServiceSelectNews :: UserId -> Role -> NewsFilters -> ServiceNews m [NewsItem]
  ServiceSetIsPublished :: UserId -> Role -> SetIsPublished -> ServiceNews m UnavailableNews
  ServiceGetCategories :: CategoryFilters -> ServiceNews m SelectedCategories
  ServiceUpdateCategories :: [CategoryName] -> ServiceNews m (Either UpdateCategoriesError ())

makeEffect ''ServiceNews