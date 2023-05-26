module Service.Types.News (
  CreateNews (..),
  EditNews (..),
) where

import Common.Prelude (Generic, Text)
import Common.TH (processRecords)
import Common.Types.News

data CreateNews = CreateNews
  { _title :: NewsTitle
  , _text :: NewsText
  , _category :: CategoryId
  , _images :: Images
  , _isPublished :: Bool
  }
  deriving (Generic)

data EditNews = EditNews
  { _id :: Int
  , _text :: Text
  , _category :: Int
  , _images :: Images
  }
  deriving (Generic)

processRecords [''CreateNews, ''EditNews]