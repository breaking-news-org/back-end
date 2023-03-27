module Service.Types.News (
  News (..),
  CreateNews (..),
  Filters,
  IndexedImages,
  news_title,
  news_creationDate,
  news_creator,
  news_category,
  news_text,
  news_id,
  news_images,
  news_isPublished,
  createNews_title,
  createNews_text,
  createNews_category,
  createNews_images,
  createNews_creator,
) where

import API.Types.News qualified as API (Filters, IndexedImages)
import Service.Prelude (Generic, Text, UTCTime, makeLenses)

-- TODO make new types?
type Filters = API.Filters
type IndexedImages = API.IndexedImages

-- | Internal
data News = News
  { _news_title :: !Text
  , _news_creationDate :: !UTCTime
  , _news_creator :: !Text
  , _news_category :: Int
  , _news_text :: !Text
  , _news_id :: Int
  , _news_images :: IndexedImages
  , _news_isPublished :: Bool
  }
  deriving (Generic)

makeLenses ''News

-- | Internal
data CreateNews = CreateNews
  { _createNews_title :: !Text
  , _createNews_text :: !Text
  , _createNews_category :: !Text
  , _createNews_images :: IndexedImages
  , _createNews_creator :: !Text
  }
  deriving (Generic)

makeLenses ''CreateNews