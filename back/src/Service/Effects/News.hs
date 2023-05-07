module Service.Effects.News where

import Effectful (Effect)
import Effectful.TH (makeEffect)
import Service.Types.News as ServiceNews (CreateNews (..), Filters (..), NewsIdHashed, SelectedNews (..), SetIsPublished, UnavailableNews, UserId, SelectedCategories)
import Service.Types.User (Role)

data ServiceNews :: Effect where
  ServiceCreateNews :: UserId -> CreateNews -> ServiceNews m NewsIdHashed
  ServiceSelectNews :: UserId -> Role -> Filters -> ServiceNews m [SelectedNews]
  ServiceSetIsPublished :: UserId -> Role -> SetIsPublished -> ServiceNews m UnavailableNews
  ServiceGetCategories :: ServiceNews m SelectedCategories

makeEffect ''ServiceNews