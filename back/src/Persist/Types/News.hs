{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Persist.Types.News where

import API.Prelude (Generic)
import Common.Prelude (Text, UTCTime)
import Common.TH (processRecords, processSums)
import Data.Default
import Data.String (IsString)
import Database.Esqueleto.Experimental (PersistField, PersistFieldSql, SqlString)
import Persist.Types.User (AuthorName, CreatedAt, UserId)

newtype Image = Image Text
  deriving (Show, Generic, Eq, Ord)

type Images = [Image]

data InsertNewsItem = InsertNewsItem
  { _insertNews_title :: !NewsTitle
  , _insertNews_createdAt :: !CreatedAt
  , _insertNews_authorId :: !UserId
  , _insertNews_category :: !CategoryId
  , _insertNews_text :: !NewsText
  , _insertNews_images :: !Images
  , _insertNews_isPublished :: !Bool
  }
  deriving (Generic)

type CategoryIds = [CategoryId]

data NewsItem = NewsItem
  { _newsItem_id :: !Int
  , _newsItem_title :: !NewsTitle
  , _newsItem_createdAt :: !CreatedAt
  , _newsItem_authorName :: !AuthorName
  , _newsItem_category :: !CategoryId
  , _newsItem_text :: !NewsText
  , _newsItem_images :: !Images
  , _newsItem_isPublished :: !Bool
  }
  deriving (Generic, Show)

data SelectedCategoryItem = SelectedCategoryItem
  { _selectedCategory_id :: CategoryId
  , _selectedCategory_name :: CategoryName
  , _selectedCategory_parent :: Maybe CategoryId
  }
  deriving (Generic)

data NewsFilters = NewsFilters
  { _newsFilters_createdUntil :: !(Maybe CreatedUntil)
  , _newsFilters_createdSince :: !(Maybe CreatedSince)
  , _newsFilters_authorName :: !(Maybe AuthorName)
  -- ^ get news by author name
  --
  -- a user can get her news when querying by her name
  , _newsFilters_categoriesInclude :: CategoryIds
  , _newsFilters_categoriesExclude :: CategoryIds
  , _newsFilters_titleLike :: !(Maybe NewsTitle)
  , _newsFilters_textLike :: !(Maybe NewsText)
  , _newsFilters_showUnpublished :: !(Maybe Bool)
  , _newsFilters_showPublished :: !(Maybe Bool)
  }
  deriving (Generic, Default)

data CategoryFilters = CategoryFilters
  { _categoryFilters_include :: CategoryIds
  , _categoryFilters_exclude :: CategoryIds
  }
  deriving (Generic, Default)

newtype CategoryName = CategoryName Text
  deriving (Generic)
  deriving newtype (PersistField, Eq, Ord, Show, PersistFieldSql, IsString, SqlString)

newtype NewsTitle = NewsTitle Text
  deriving (Generic)
  deriving newtype (PersistField, Eq, Ord, Show, PersistFieldSql, IsString, SqlString, Semigroup)

newtype NewsText = NewsText Text
  deriving (Generic)
  deriving newtype (PersistField, Eq, Ord, Show, PersistFieldSql, IsString, SqlString, Semigroup)

-- | News id is hashed to prevent brute-forcing all news
newtype NewsId = NewsId Int
  deriving (Generic)
  deriving newtype (Num, Integral, Enum, Real, PersistField, Eq, Ord, Show, PersistFieldSql)

newtype CategoryId = CategoryId Int
  deriving (Generic)
  deriving newtype (Num, Integral, Enum, Real, PersistField, Eq, Ord, Show, PersistFieldSql)

newtype CreatedSince = CreatedSince UTCTime
  deriving (Generic)
  deriving newtype (PersistField, Eq, Ord, Show, PersistFieldSql)

newtype CreatedUntil = CreatedUntil UTCTime
  deriving (Generic)
  deriving newtype (PersistField, Eq, Ord, Show, PersistFieldSql)

type UnavailableNews = [NewsId]

type SelectedCategories = [SelectedCategoryItem]

newtype InsertNewsError
  = -- | An author unregistered, but then decided to create a news by a not-yet-expired access token
    AuthorDoesNotExist UserId
  deriving (Generic, Show)

data SetIsPublished = SetIsPublished
  { _setIsPublished_newsIds :: ![NewsId]
  , _setIsPublished_isPublished :: !Bool
  }
  deriving (Generic)

processRecords
  [ ''Image
  , ''InsertNewsItem
  , ''NewsTitle
  , ''NewsText
  , ''NewsId
  , ''SetIsPublished
  , ''NewsItem
  , ''SelectedCategoryItem
  , ''CategoryName
  , ''CategoryId
  , ''CreatedUntil
  , ''CreatedSince
  ]

processSums
  [ ''InsertNewsError
  ]