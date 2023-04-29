module API.Types.News (
  CreateNews (..),
  EditNews (..),
  GetNews (..),
  module Service.Types.News,
  QueryParams (..),
) where

import API.Prelude (Generic, UTCTime)
import API.TH (makeToSchema, processApiRecord)
import Common.Prelude (Text)

import Data.Default (Default)
import Service.Types.News (Filters (..), GetNews (..), Image (..), Images)
import API.Types.User ()

makeToSchema ''Image

data CreateNews = CreateNews
  { _createNews_title :: !Text
  , _createNews_text :: !Text
  , _createNews_category :: Int
  , _createNews_images :: Images
  }
  deriving (Generic)

processApiRecord ''CreateNews

data EditNews = EditNews
  { _editNews_id :: Int
  , _editNews_text :: !Text
  , _editNews_category :: Int
  , _editNews_images :: Images
  }
  deriving (Generic)

processApiRecord ''EditNews

makeToSchema ''GetNews

data QueryParams = QueryParams
  { _queryParams_createdUntil :: Maybe UTCTime
  , _queryParams_createdSince :: Maybe UTCTime
  , _queryParams_createdAt :: Maybe UTCTime
  , _queryParams_authorName :: Maybe Text
  , _queryParams_category :: Maybe Int
  , _queryParams_content :: Maybe Text
  , _queryParams_block :: Maybe Int
  , _queryParams_newsId :: Maybe Int
  }
  deriving (Generic)

processApiRecord ''QueryParams

instance Default QueryParams