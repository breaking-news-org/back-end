{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

module API.Types.News where

import API.Prelude
import API.TH (processApiRecord)
import Common.Prelude (Generic, Text)
import Database.Esqueleto.Experimental (PersistFieldSql (sqlType), SqlType (SqlString))

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
  { _getNews_id :: Int
  , _getNews_text :: !Text
  , _getNews_category :: !Text
  , _getNews_images :: IndexedImages
  }
  deriving (Generic)

processApiRecord ''GetNews
