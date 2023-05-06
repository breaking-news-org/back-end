module Service.Effects.News where

import Effectful (Effect)
import Effectful.TH (makeEffect)
import Service.Types.News as ServiceNews (CreateNews (..), Filters (..), NewsIdHashed, SelectedNews (..), SetIsPublished, UnavailableNews)
import Service.Types.User (AccessToken)

data ServiceNews :: Effect where
  ServiceCreateNews :: AccessToken -> CreateNews -> ServiceNews m NewsIdHashed
  ServiceSelectNews :: AccessToken -> Filters -> ServiceNews m [SelectedNews]
  ServiceSetIsPublished :: AccessToken -> SetIsPublished -> ServiceNews m UnavailableNews

makeEffect ''ServiceNews