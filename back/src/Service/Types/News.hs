module Service.Types.News (
  CreateNews (..),
  module Persist.Types.News,
  module Persist.Types.User,
) where

import Common.Prelude (Generic)
import Persist.Types.News (CategoryName, Filters (..), Image (..), Images, NewsIdHashed (..), NewsText (..), NewsTitle (..), SelectedCategories, SelectedCategory, SelectedNews (..), SetIsPublished (..), UnavailableNews)
import Persist.Types.User (AuthorName (..), CategoryId (..), CreatedAt (..), CreatedSince (..), CreatedUntil (..), UserId (..))

data CreateNews = CreateNews
  { _createNews_title :: !NewsTitle
  , _createNews_text :: !NewsText
  , _createNews_category :: !CategoryId
  , _createNews_images :: !Images
  , _createNews_isPublished :: Bool
  }
  deriving (Generic)