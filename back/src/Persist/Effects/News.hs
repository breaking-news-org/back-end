module Persist.Effects.News where

import Effectful (Effect)
import Effectful.TH (makeEffect)
import Persist.Types.News (Filters, InsertNews, SelectedNews)

data NewsRepo :: Effect where
  RepoInsertNews :: InsertNews -> NewsRepo m ()
  RepoSelectedNews :: Filters Maybe -> NewsRepo m [SelectedNews]

makeEffect ''NewsRepo