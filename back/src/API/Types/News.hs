{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module API.Types.News where

import API.Prelude
import API.TH (processApiRecord, processApiRecord')
import Common.Prelude (HKD, Text)
import Database.Esqueleto.Experimental (PersistFieldSql (sqlType), SqlType (SqlString))
import Data.Int (Int64)

data IndexedImage = IndexedImages
  { _indexedImage_id :: Int
  , _indexedImage_value :: !Text
  }
  deriving (Show, Generic, Eq, Ord)

processApiRecord ''IndexedImage

instance PersistField IndexedImage where
  fromPersistValue = fromPersistValueJSON
  toPersistValue = toPersistValueJSON

instance PersistFieldSql IndexedImage where
  sqlType _ = SqlString

type IndexedImages = [IndexedImage]

-- | Transport
data CreateNews = CreateNews
  { _createNews_title :: !Text
  , _createNews_text :: !Text
  , _createNews_category :: !Text
  , _createNews_images :: IndexedImages
  }
  deriving (Generic)

processApiRecord ''CreateNews

-- | Transport
data EditNews = EditNews
  { _editNews_id :: Int
  , _editNews_text :: !Text
  , _editNews_category :: !Text
  , _editNews_images :: IndexedImages
  }
  deriving (Generic)

processApiRecord ''EditNews

-- | Transport
data GetNews = GetNews
  { _getNews_id :: Maybe Int
  , _getNews_category :: Maybe Text
  }
  deriving (Generic)

processApiRecord ''GetNews

-- | Transport
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

processApiRecord ''News

data Filters f = Filters
  { _filters_createdUntil :: HKD f UTCTime
  , _filters_createdSince :: HKD f UTCTime
  , _filters_createdAt :: HKD f UTCTime
  , _filters_creator :: HKD f Text
  , _filters_categoryId :: HKD f Int
  , _filters_content :: HKD f Text
  , _filters_block :: HKD f Int64
  }
  deriving (Generic)

processApiRecord' [''Filters, ''Maybe]