module Persist.Types.News where

import API.Prelude (Generic, UTCTime)
import Common.Prelude (HKD, Text)
import Common.TH (processTypes)
import Persist.Types.User (CategoryId, UserId, AuthorName)

newtype Image = Image Text
  deriving (Show, Generic, Eq, Ord)

type Images = [Image]

data InsertNews = InsertNews
  { _insertNews_title :: !Text
  , _insertNews_creationDate :: !UTCTime
  , _insertNews_authorId :: !UserId
  , _insertNews_category :: !CategoryId
  , _insertNews_text :: !Text
  , _insertNews_images :: !Images
  , _insertNews_isPublished :: !Bool
  }
  deriving (Generic)

data SelectedNews = SelectedNews
  { _selectNews_id :: !Int
  , _selectNews_title :: !Text
  , _selectNews_creationDate :: !UTCTime
  , _selectNews_authorName :: !AuthorName
  , _selectNews_category :: !CategoryId
  , _selectNews_text :: !Text
  , _selectNews_images :: !Images
  , _selectNews_isPublished :: !Bool
  }
  deriving (Generic)

data Filters f = Filters
  { _filters_createdUntil :: !(HKD f UTCTime)
  , _filters_createdSince :: !(HKD f UTCTime)
  , _filters_createdAt :: !(HKD f UTCTime)
  , _filters_authorName :: !(HKD f AuthorName)
  , _filters_category :: !(HKD f CategoryId)
  , _filters_content :: !(HKD f Text)
  , _filters_block :: !(HKD f Int)
  , _filters_newsId :: !(HKD f Int)
  , _filters_showUnpublished :: !(HKD f Bool)
  -- TODO make separate filters for API?
  }
  deriving (Generic)

processTypes [''Image, ''InsertNews]