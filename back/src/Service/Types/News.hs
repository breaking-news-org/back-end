module Service.Types.News (
  GetNews (..),
  CreateNews (..),
  module Persist.Types.News,
) where

import Common.Prelude (Generic)
import Common.TH (processRecord)
import Persist.Types.News (Filters (..), Image (..), Images, NewsText, Title)
import Persist.Types.User (AuthorName, CategoryId, CreatedAt, UserId)

data GetNews = GetNews
  { _getNews_title :: !Title
  , _getNews_createdAt :: !CreatedAt
  , _getNews_authorName :: !AuthorName
  , _getNews_category :: !CategoryId
  , _getNews_text :: !NewsText
  , _getNews_images :: Images
  , _getNews_isPublished :: Bool
  }
  deriving (Generic, Show)

processRecord ''GetNews

data CreateNews = CreateNews
  { _createNews_title :: !Title
  , _createNews_text :: !NewsText
  , _createNews_category :: !CategoryId
  , _createNews_images :: !Images
  , _createNews_userId :: !UserId
  }
  deriving (Generic)