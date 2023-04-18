module Service.Types.News (
  GetNews (..),
  getNews_title,
  getNews_creationDate,
  getNews_creator,
  getNews_category,
  getNews_text,
  getNews_id,
  getNews_images,
  getNews_isPublished,
  CreateNews (..),
  createNews_title,
  createNews_text,
  createNews_category,
  createNews_images,
  createNews_creator,
  module Persist.Types.News,
) where

import Common.Prelude (Generic, Text, UTCTime, makeLenses)
import Common.TH (makeFromToJSON)
import Persist.Types.News (Filters (..), IndexedImage (..), IndexedImages, filters_block, filters_category, filters_content, filters_createdAt, filters_createdSince, filters_createdUntil, filters_creator, filters_newsId)

data GetNews = GetNews
  { _getNews_title :: !Text
  , _getNews_creationDate :: !UTCTime
  , _getNews_creator :: !Text
  , _getNews_category :: Int
  , _getNews_text :: !Text
  , _getNews_id :: Int
  , _getNews_images :: IndexedImages
  , _getNews_isPublished :: Bool
  }
  deriving (Generic, Show)

makeLenses ''GetNews

makeFromToJSON ''GetNews

data CreateNews = CreateNews
  { _createNews_title :: !Text
  , _createNews_text :: !Text
  , _createNews_category :: Int
  , _createNews_images :: IndexedImages
  , _createNews_creator :: !Text
  }
  deriving (Generic)

makeLenses ''CreateNews
