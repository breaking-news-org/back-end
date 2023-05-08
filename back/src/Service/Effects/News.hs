module Service.Effects.News where

import Effectful (Effect)
import Effectful.TH (makeEffect)
import Service.Types.News as ServiceNews (CategoryFilters, CreateNews (..), NewsFilters (..), NewsItem (..), SelectedCategories, SetIsPublished, UnavailableNews, UserId, InsertNewsError)
import Service.Types.User (Role)

data ServiceNews :: Effect where
  ServiceCreateNews :: UserId -> CreateNews -> ServiceNews m (Either InsertNewsError NewsItem)
  ServiceSelectNews :: UserId -> Role -> NewsFilters -> ServiceNews m [NewsItem]
  ServiceSetIsPublished :: UserId -> Role -> SetIsPublished -> ServiceNews m UnavailableNews
  ServiceGetCategories :: CategoryFilters -> ServiceNews m SelectedCategories

makeEffect ''ServiceNews