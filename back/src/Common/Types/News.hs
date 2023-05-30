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
  { _title :: NewsTitle
  , _createdAt :: CreatedAt
  , _authorId :: UserId
  , _categories :: [CategoryName]
  , _text :: NewsText
  , _images :: Images
  , _isPublished :: Bool
  }
  deriving (Generic)

data NewsItem = NewsItem
  { _id :: Int
  , _title :: NewsTitle
  , _createdAt :: CreatedAt
  , _authorName :: AuthorName
  , _categories :: [CategoryName]
  , _text :: NewsText
  , _images :: Images
  , _isPublished :: Bool
  }
  deriving (Generic, Show)

data SelectedCategoryItem = SelectedCategoryItem
  { _id :: CategoryId
  , _name :: CategoryName
  , _parent :: Maybe CategoryId
  }
  deriving (Generic, Show)

data NewsFilters = NewsFilters
  { _createdUntil :: Maybe CreatedUntil
  , _createdSince :: Maybe CreatedSince
  , _authorName :: Maybe AuthorName
  , _categoriesInclude :: [CategoryId]
  , _categoriesExclude :: [CategoryId]
  , _titleLike :: Maybe NewsTitle
  , _textLike :: Maybe NewsText
  , _showUnpublished :: Maybe Bool
  , _showPublished :: Maybe Bool
  }
  deriving (Generic)

instance Default NewsFilters where
  def :: NewsFilters
  def =
    NewsFilters
      { _createdUntil = def
      , _createdSince = def
      , _authorName = def
      , _categoriesInclude = def
      , _categoriesExclude = def
      , _titleLike = def
      , _textLike = def
      , _showUnpublished = def
      , _showPublished = def
      }

data CategoryFilters = CategoryFilters
  { _include :: [CategoryId]
  , _exclude :: [CategoryId]
  }
  deriving (Generic)

instance Default CategoryFilters where
  def :: CategoryFilters
  def =
    CategoryFilters
      { _include = def
      , _exclude = def
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

data InsertNewsError
  = -- | An author unregistered, but then decided to create a news by a not-yet-expired access token
    AuthorDoesNotExist UserId
  | InvalidCategories [CategoryName]
  deriving (Generic, Show)

data UpdateCategoriesError = InvalidCategories' [CategoryName]

data SetIsPublished = SetIsPublished
  { _newsIds :: [NewsId]
  , _isPublished :: Bool
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

t :: Maybe Images
t = decode $ encode [Image "ha"]

-- >>> t
-- Just [Image "ha"]
