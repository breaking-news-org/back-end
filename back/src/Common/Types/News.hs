module Common.Types.News where

import Common.Prelude
import Common.TH
import Common.Types.User
import Data.Aeson (decode, encode)
import Data.Default (Default (def))
import Data.String
import Database.Esqueleto.Experimental

newtype Image = Image Text
  deriving (Show, Generic, Eq, Ord)

type Images = [Image]

newtype CreatedAt = CreatedAt UTCTime
  deriving (Generic)
  deriving newtype (PersistField, Eq, Ord, Show, PersistFieldSql)

data InsertNewsItem = InsertNewsItem
  { _insertNews_title :: NewsTitle
  , _insertNews_createdAt :: CreatedAt
  , _insertNews_authorId :: UserId
  , _insertNews_category :: CategoryId
  , _insertNews_text :: NewsText
  , _insertNews_images :: Images
  , _insertNews_isPublished :: Bool
  }
  deriving (Generic)

type CategoryIds = [CategoryId]

data NewsItem = NewsItem
  { _newsItem_id :: Int
  , _newsItem_title :: NewsTitle
  , _newsItem_createdAt :: CreatedAt
  , _newsItem_authorName :: AuthorName
  , _newsItem_category :: CategoryId
  , _newsItem_text :: NewsText
  , _newsItem_images :: Images
  , _newsItem_isPublished :: Bool
  }
  deriving (Generic, Show)

data SelectedCategoryItem = SelectedCategoryItem
  { _selectedCategory_id :: CategoryId
  , _selectedCategory_name :: CategoryName
  , _selectedCategory_parent :: Maybe CategoryId
  }
  deriving (Generic, Show)

data NewsFilters = NewsFilters
  { _newsFilters_createdUntil :: Maybe CreatedUntil
  , _newsFilters_createdSince :: Maybe CreatedSince
  , _newsFilters_authorName :: Maybe AuthorName
  , _newsFilters_categoriesInclude :: CategoryIds
  , _newsFilters_categoriesExclude :: CategoryIds
  , _newsFilters_titleLike :: Maybe NewsTitle
  , _newsFilters_textLike :: Maybe NewsText
  , _newsFilters_showUnpublished :: Maybe Bool
  , _newsFilters_showPublished :: Maybe Bool
  }
  deriving (Generic)

instance Default NewsFilters where
  def :: NewsFilters
  def =
    NewsFilters
      { _newsFilters_createdUntil = def
      , _newsFilters_createdSince = def
      , _newsFilters_authorName = def
      , _newsFilters_categoriesInclude = def
      , _newsFilters_categoriesExclude = def
      , _newsFilters_titleLike = def
      , _newsFilters_textLike = def
      , _newsFilters_showUnpublished = def
      , _newsFilters_showPublished = def
      }

data CategoryFilters = CategoryFilters
  { _categoryFilters_include :: CategoryIds
  , _categoryFilters_exclude :: CategoryIds
  }
  deriving (Generic)

instance Default CategoryFilters where
  def :: CategoryFilters
  def =
    CategoryFilters
      { _categoryFilters_include = def
      , _categoryFilters_exclude = def
      }

newtype CategoryName = CategoryName Text
  deriving (Generic)
  deriving newtype (PersistField, Eq, Ord, Show, PersistFieldSql, IsString, SqlString)

newtype NewsTitle = NewsTitle Text
  deriving (Generic)
  deriving newtype (PersistField, Eq, Ord, Show, PersistFieldSql, IsString, SqlString, Semigroup)

newtype NewsText = NewsText Text
  deriving (Generic)
  deriving newtype (PersistField, Eq, Ord, Show, PersistFieldSql, IsString, SqlString, Semigroup)

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
  { _setIsPublished_newsIds :: [NewsId]
  , _setIsPublished_isPublished :: Bool
  }
  deriving (Generic)

processRecords
  [ ''InsertNewsItem
  , ''NewsItem
  , ''SetIsPublished
  , ''SelectedCategoryItem
  , ''NewsFilters
  , ''CategoryFilters
  , ''Image
  , ''NewsTitle
  , ''NewsText
  , ''NewsId
  , ''CategoryName
  , ''CategoryId
  , ''CreatedUntil
  , ''CreatedSince
  , ''CreatedAt
  ]

processSums
  [ ''InsertNewsError
  ]

-- t :: Maybe Images
-- t = decode $ encode $ [Image "ha"]

-- >>> t
-- Just [Image "ha"]
