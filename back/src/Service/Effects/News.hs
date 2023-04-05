module Service.Effects.News where

import Effectful (Effect)
import Effectful.TH (makeEffect)
import Service.Types.News as ServiceNews (CreateNews (..), Filters (..), GetNews (..))

-- TODO
-- if a news is unpublished it can only be shown to its author

data ServiceNews :: Effect where
  ServiceCreateNews :: CreateNews -> ServiceNews m ()
  ServiceGetNews :: Filters Maybe -> ServiceNews m [GetNews]

makeEffect ''ServiceNews