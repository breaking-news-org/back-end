module Service.Effects.News where

import Effectful (Effect)
import Effectful.TH (makeEffect)
import Persist.Types.User (SessionId)

data UserService :: Effect where
  ServiceCreateSession :: UserService m SessionId

makeEffect ''UserService
