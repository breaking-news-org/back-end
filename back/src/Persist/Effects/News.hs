module Persist.Effects.News where

import Effectful
import Effectful.TH
import Common.Types.News
import Common.Types.User

data NewsRepo :: Effect where
  RepoInsertNewsItem :: InsertNewsItem -> NewsRepo m (Either InsertNewsError NewsItem)
  RepoSelectNews :: UserId -> Role -> NewsFilters -> NewsRepo m [NewsItem]
  RepoUpdateIsPublished :: UserId -> Role -> SetIsPublished -> NewsRepo m UnavailableNews
  RepoSelectCategoryIds :: CategoryFilters -> NewsRepo m SelectedCategories

makeEffect ''NewsRepo