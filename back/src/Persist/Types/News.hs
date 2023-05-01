{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Persist.Types.News where

import API.Prelude (Generic)
import Common.Prelude (HKD, Text)
import Common.TH (processRecords, processSums)
import Data.String (IsString)
import Database.Esqueleto.Experimental (PersistField, PersistFieldSql, SqlString)
import Persist.Types.User (AuthorName, CategoryId, CreatedAt, CreatedSince, CreatedUntil, UserId)
import Servant.API (FromHttpApiData, ToHttpApiData)

newtype Image = Image Text
  deriving (Show, Generic, Eq, Ord)

type Images = [Image]

data InsertNews = InsertNews
  { _insertNews_title :: !Title
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
  , _selectNews_title :: !Title
  , _selectNews_createdAt :: !CreatedAt
  , _selectNews_authorName :: !AuthorName
  , _selectNews_category :: !CategoryId
  , _selectNews_text :: !NewsText
  , _selectNews_images :: !Images
  , _selectNews_isPublished :: !Bool
  }
  deriving (Generic)

data Filters f = Filters
  { _filters_createdUntil :: !(HKD f CreatedUntil)
  , _filters_createdSince :: !(HKD f CreatedSince)
  , _filters_createdAt :: !(HKD f CreatedAt)
  , _filters_authorName :: !(HKD f AuthorName)
  , _filters_category :: !(HKD f CategoryId)
  , _filters_titleLike :: !(HKD f Title)
  , _filters_textLike :: !(HKD f NewsText)
  , _filters_block :: !(HKD f Int)
  , _filters_showUnpublished :: !(HKD f Bool)
  }
  deriving (Generic)

newtype Title = Title Text
  deriving (Generic)
  deriving newtype (PersistField, Eq, Ord, Show, PersistFieldSql, IsString, SqlString, FromHttpApiData, ToHttpApiData)

newtype NewsText = NewsText Text
  deriving (Generic)
  deriving newtype (PersistField, Eq, Ord, Show, PersistFieldSql, IsString, SqlString, FromHttpApiData, ToHttpApiData)

-- processSums []
processRecords [''Image, ''InsertNews, ''Title, ''NewsText]

