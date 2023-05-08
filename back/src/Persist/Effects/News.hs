module Persist.Effects.News where

import Effectful (Effect)
import Effectful.TH (makeEffect)
import Persist.Types.News (CategoryFilters, InsertNewsItem, NewsFilters, NewsItem, SelectedCategories, SetIsPublished, UnavailableNews, InsertNewsError)
import Persist.Types.User (Role, UserId)

data NewsRepo :: Effect where
  RepoInsertNewsItem :: InsertNewsItem -> NewsRepo m (Either InsertNewsError NewsItem)
  RepoSelectNews :: UserId -> NewsFilters -> NewsRepo m [NewsItem]
  RepoUpdateIsPublished :: UserId -> Role -> SetIsPublished -> NewsRepo m UnavailableNews
  RepoSelectCategoryIds :: CategoryFilters -> NewsRepo m SelectedCategories

makeEffect ''NewsRepo