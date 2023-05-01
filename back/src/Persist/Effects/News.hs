module Persist.Effects.News where

import Effectful (Effect)
import Effectful.TH (makeEffect)
import Persist.Types.News (Filters, InsertNews, SelectedNews)
import Persist.Types.User (UserId)

data NewsRepo :: Effect where
  RepoInsertNews :: InsertNews -> NewsRepo m ()
  RepoSelectNews :: Filters -> UserId -> NewsRepo m [SelectedNews]

makeEffect ''NewsRepo