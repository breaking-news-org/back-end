module Persist.Effects.News where

import Effectful
import Effectful.TH
import Common.Types.News
import Common.Types.User

data NewsRepo :: Effect where
  -- TODO return news id (e.g., UUID or kebab-cased title)
  RepoInsertNewsItem :: InsertNewsItem -> NewsRepo m (Either InsertNewsError ())
  RepoSelectNews :: UserId -> Role -> NewsFilters -> NewsRepo m [NewsItem]
  RepoUpdateIsPublished :: UserId -> Role -> SetIsPublished -> NewsRepo m UnavailableNews
  RepoSelectCategoryIds :: CategoryFilters -> NewsRepo m SelectedCategories
  RepoUpdateCategories :: [CategoryName] -> NewsRepo m ()

makeEffect ''NewsRepo