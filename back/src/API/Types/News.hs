module API.Types.News (
  CreateNews (..),
  EditNews (..),
  GetNews (..),
  module Service.Types.News,
  QueryParams (..),
) where

import API.Prelude (Generic, UTCTime)
import API.TH (makeRecordToSchemaTypes, processRecordApiTypes)
import Common.Prelude (Text)

import API.Types.User (AuthorName)
import Data.Default (Default)
import Service.Types.News (Filters (..), GetNews (..), Image (..), Images)
import Service.Types.User (CategoryId)

data CreateNews = CreateNews
  { _createNews_title :: !Text
  , _createNews_text :: !Text
  , _createNews_category :: CategoryId
  , _createNews_images :: Images
  }
  deriving (Generic)

data EditNews = EditNews
  { _editNews_id :: Int
  , _editNews_text :: !Text
  , _editNews_category :: Int
  , _editNews_images :: Images
  }
  deriving (Generic)

data QueryParams = QueryParams
  { _queryParams_createdUntil :: Maybe UTCTime
  , _queryParams_createdSince :: Maybe UTCTime
  , _queryParams_createdAt :: Maybe UTCTime
  , _queryParams_authorName :: Maybe AuthorName
  , _queryParams_category :: Maybe CategoryId
  , _queryParams_content :: Maybe Text
  , _queryParams_block :: Maybe Int
  , _queryParams_newsId :: Maybe Int
  }
  deriving (Generic)

instance Default QueryParams

makeRecordToSchemaTypes [''GetNews, ''Image]

processRecordApiTypes [''EditNews, ''CreateNews, ''QueryParams]
