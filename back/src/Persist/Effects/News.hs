module Persist.Effects.News where

import Effectful (Effect)
import Effectful.TH (makeEffect)
import Persist.Types.News (Filters, InsertNews, SelectNews)

data NewsRepo :: Effect where
  RepoInsertNews :: InsertNews -> NewsRepo m ()
  RepoSelectNews :: Filters Maybe -> NewsRepo m [SelectNews]

makeEffect ''NewsRepo