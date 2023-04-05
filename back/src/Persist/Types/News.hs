module Persist.Types.News where

import Common.Prelude (Generic, HKD, Text, UTCTime)
import Common.TH (makeLenses, processRecord)
import Database.Esqueleto.Experimental (PersistField (..), PersistFieldSql (sqlType), SqlType (SqlString), fromPersistValue, fromPersistValueJSON, toPersistValueJSON)

data IndexedImage = IndexedImages
  { _indexedImage_id :: Int
  , _indexedImage_value :: !Text
  }
  deriving (Show, Generic, Eq, Ord)

processRecord ''IndexedImage

instance PersistField IndexedImage where
  fromPersistValue = fromPersistValueJSON
  toPersistValue = toPersistValueJSON

instance PersistFieldSql IndexedImage where
  sqlType _ = SqlString

type IndexedImages = [IndexedImage]

data InsertNews = InsertNews
  { _insertNews_title :: !Text
  , _insertNews_creationDate :: !UTCTime
  , _insertNews_creator :: !Text
  , _insertNews_category :: Int
  , _insertNews_text :: !Text
  , _insertNews_images :: IndexedImages
  , _insertNews_isPublished :: Bool
  }
  deriving (Generic)

makeLenses ''InsertNews

data SelectNews = SelectNews
  { _selectNews_id :: Int
  , _selectNews_title :: !Text
  , _selectNews_creationDate :: !UTCTime
  , _selectNews_creator :: !Text
  , _selectNews_category :: Int
  , _selectNews_text :: !Text
  , _selectNews_images :: IndexedImages
  , _selectNews_isPublished :: Bool
  }
  deriving (Generic)

makeLenses ''SelectNews

data Filters f = Filters
  { _filters_createdUntil :: HKD f UTCTime
  , _filters_createdSince :: HKD f UTCTime
  , _filters_createdAt :: HKD f UTCTime
  , _filters_creator :: HKD f Text
  , _filters_category :: HKD f Int
  , _filters_content :: HKD f Text
  , _filters_block :: HKD f Int
  , _filters_newsId :: HKD f Int
  }
  deriving (Generic)

makeLenses ''Filters