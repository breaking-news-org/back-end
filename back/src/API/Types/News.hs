module API.Types.News (
  CreateNews (..),
  createNews_title,
  createNews_text,
  createNews_category,
  createNews_images,
  EditNews (..),
  editNews_id,
  editNews_text,
  editNews_category,
  editNews_images,
  GetNews (..),
  module Service.Types.News,
  QueryParams (..),
  queryParams_createdUntil,
  queryParams_createdSince,
  queryParams_createdAt,
  queryParams_creator,
  queryParams_category,
  queryParams_content,
  queryParams_block,
  queryParams_newsId,
) where

import API.Prelude (Generic, UTCTime)
import API.TH (makeToSchema, processApiRecord)
import Common.Prelude (Text)

import Service.Types.News (Filters (..), GetNews (..), IndexedImage (..), IndexedImages)
import Data.Default (Default)

makeToSchema ''IndexedImage

makeToSchema ''IndexedImages

data CreateNews = CreateNews
  { _createNews_title :: !Text
  , _createNews_text :: !Text
  , _createNews_category :: Int
  , _createNews_images :: IndexedImages
  }
  deriving (Generic)

processApiRecord ''CreateNews

data EditNews = EditNews
  { _editNews_id :: Int
  , _editNews_text :: !Text
  , _editNews_category :: Int
  , _editNews_images :: IndexedImages
  }
  deriving (Generic)

processApiRecord ''EditNews

makeToSchema ''GetNews

data QueryParams = QueryParams
  { _queryParams_createdUntil :: Maybe UTCTime
  , _queryParams_createdSince :: Maybe UTCTime
  , _queryParams_createdAt :: Maybe UTCTime
  , _queryParams_creator :: Maybe Text
  , _queryParams_category :: Maybe Int
  , _queryParams_content :: Maybe Text
  , _queryParams_block :: Maybe Int
  , _queryParams_newsId :: Maybe Int
  }
  deriving (Generic)

processApiRecord ''QueryParams

instance Default QueryParams