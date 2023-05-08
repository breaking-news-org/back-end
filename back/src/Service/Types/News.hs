module Service.Types.News (
  CreateNews (..),
  EditNews (..),
  module Persist.Types.News,
  module Persist.Types.User,
) where

import Common.Prelude (Generic, Text)
import Persist.Types.News
import Persist.Types.User

data CreateNews = CreateNews
  { _createNews_title :: !NewsTitle
  , _createNews_text :: !NewsText
  , _createNews_category :: !CategoryId
  , _createNews_images :: !Images
  , _createNews_isPublished :: Bool
  }
  deriving (Generic)

data EditNews = EditNews
  { _editNews_id :: !Int
  , _editNews_text :: !Text
  , _editNews_category :: !Int
  , _editNews_images :: !Images
  }
  deriving (Generic)