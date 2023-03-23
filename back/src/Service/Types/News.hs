module Service.Types.News where

import API.Types.News (IndexedImages)
import Service.Prelude

-- | Internal
data News = News
  { _news_title :: !Text
  , _news_creationDate :: !UTCTime
  , _news_creator :: !Text
  , _news_category :: !Text
  , _news_text :: !Text
  , _news_id :: Int
  , _news_images :: IndexedImages
  , _news_isPublished :: Bool
  }
  deriving (Generic)

makeLenses ''News

-- | Internal
data PutNews = PutNews
  { _putNews_title :: !Text
  , _putNews_text :: !Text
  , _putNews_category :: !Text
  , _putNews_images :: IndexedImages
  , _putNews_creator :: !Text
  }
  deriving (Generic)

makeLenses ''PutNews