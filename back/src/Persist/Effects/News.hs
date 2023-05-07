module Persist.Effects.News where

import Effectful (Effect)
import Effectful.TH (makeEffect)
import Persist.Types.News (Filters, InsertNews, NewsIdHashed, SelectedNews, SetIsPublished, UnavailableNews, SelectedCategories)
import Persist.Types.User (Role, UserId)

data NewsRepo :: Effect where
  RepoInsertNews :: InsertNews -> NewsRepo m NewsIdHashed
  RepoSelectNews :: UserId -> Filters -> NewsRepo m [SelectedNews]
  RepoUpdateIsPublished :: UserId -> Role -> SetIsPublished -> NewsRepo m UnavailableNews
  RepoSelectCategories :: NewsRepo m SelectedCategories

makeEffect ''NewsRepo