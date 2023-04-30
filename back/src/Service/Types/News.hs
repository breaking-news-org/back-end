module Service.Types.News (
  GetNews (..),
  CreateNews (..),
  module Persist.Types.News,
) where

import Common.Prelude (Generic, Text, UTCTime)
import Common.TH (processType)
import Persist.Types.News (Filters (..), Image (..), Images)
import Persist.Types.User (AuthorName, UserId, CategoryId)

data GetNews = GetNews
  { _getNews_title :: !Text
  , _getNews_creationDate :: !UTCTime
  , _getNews_authorName :: !AuthorName
  , _getNews_category :: !CategoryId
  , _getNews_text :: !Text
  , _getNews_id :: Int
  , _getNews_images :: Images
  , _getNews_isPublished :: Bool
  }
  deriving (Generic, Show)

processType ''GetNews

data CreateNews = CreateNews
  { _createNews_title :: !Text
  , _createNews_text :: !Text
  , _createNews_category :: !CategoryId
  , _createNews_images :: !Images
  , _createNews_userId :: !UserId
  }
  deriving (Generic)