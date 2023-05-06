{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Persist.Types.News where

import API.Prelude (Generic)
import Common.Prelude (Text)
import Common.TH (processRecords)
import Data.String (IsString)
import Database.Esqueleto.Experimental (PersistField, PersistFieldSql, SqlString)
import Persist.Types.User (AuthorName, CategoryId, CreatedAt, CreatedSince, CreatedUntil, UserId)
import Servant.API (FromHttpApiData, ToHttpApiData)

newtype Image = Image Text
  deriving (Show, Generic, Eq, Ord)

type Images = [Image]

data InsertNews = InsertNews
  { _insertNews_title :: !NewsTitle
  , _insertNews_createdAt :: !CreatedAt
  , _insertNews_authorId :: !UserId
  , _insertNews_category :: !CategoryId
  , _insertNews_text :: !NewsText
  , _insertNews_images :: !Images
  , _insertNews_isPublished :: !Bool
  }
  deriving (Generic)

data SelectedNews = SelectedNews
  { _selectNews_id :: !Int
  , _selectNews_title :: !NewsTitle
  , _selectNews_createdAt :: !CreatedAt
  , _selectNews_authorName :: !AuthorName
  , _selectNews_category :: !CategoryId
  , _selectNews_text :: !NewsText
  , _selectNews_images :: !Images
  , _selectNews_isPublished :: !Bool
  }
  deriving (Generic, Show)

data SetIsPublished = SetIsPublished
  { _setIsPublished_news :: ![NewsIdHashed]
  , _setIsPublished_isPublished :: !Bool
  }
  deriving (Generic)

data Filters = Filters
  { _filters_createdUntil :: !(Maybe CreatedUntil)
  , _filters_createdSince :: !(Maybe CreatedSince)
  , _filters_createdAt :: !(Maybe CreatedAt)
  , _filters_authorName :: !(Maybe AuthorName)
  , _filters_category :: !(Maybe CategoryId)
  , _filters_titleLike :: !(Maybe NewsTitle)
  , _filters_textLike :: !(Maybe NewsText)
  , _filters_block :: !(Maybe Int)
  , _filters_showUnpublished :: !(Maybe Bool)
  }
  deriving (Generic)

newtype NewsTitle = NewsTitle Text
  deriving (Generic)
  deriving newtype (PersistField, Eq, Ord, Show, PersistFieldSql, IsString, SqlString, FromHttpApiData, ToHttpApiData)

newtype NewsText = NewsText Text
  deriving (Generic)
  deriving newtype (PersistField, Eq, Ord, Show, PersistFieldSql, IsString, SqlString, FromHttpApiData, ToHttpApiData)

-- | News id is hashed to prevent brute-forcing all news
newtype NewsIdHashed = NewsIdHashed Text
  deriving (Generic)
  deriving newtype (PersistField, Eq, Ord, Show, PersistFieldSql, IsString, SqlString, FromHttpApiData, ToHttpApiData)

processRecords [''Image, ''InsertNews, ''NewsTitle, ''NewsText, ''NewsIdHashed, ''SetIsPublished]
