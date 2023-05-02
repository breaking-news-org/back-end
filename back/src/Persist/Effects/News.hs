module Persist.Effects.News where

import Effectful (Effect)
import Effectful.TH (makeEffect)
import Persist.Types.News (Filters, InsertNews, SelectedNews, NewsIdHashed)
import Persist.Types.User (AccessToken)
import Service.Types.News (SetIsPublished, UnavailableNews)

data NewsRepo :: Effect where
  RepoInsertNews :: InsertNews -> NewsRepo m NewsIdHashed
  RepoSelectNews :: AccessToken -> Filters -> NewsRepo m [SelectedNews]
  RepoUpdateIsPublished :: AccessToken -> SetIsPublished -> NewsRepo m UnavailableNews

makeEffect ''NewsRepo