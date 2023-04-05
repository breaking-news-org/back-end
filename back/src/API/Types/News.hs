module API.Types.News (
  CreateNews (..),
  createNews_title,
  createNews_text,
  createNews_category,
  createNews_images,
  EditNews (..),
  editNews_id,
  editNews_text,
  editNews_category,
  editNews_images,
  GetNews (..),
  module Service.Types.News,
) where

import API.Prelude (Generic)
import API.TH (makeToSchema, processApiRecord)
import Common.Prelude (Text)

import Service.Types.News (Filters (..), GetNews (..), IndexedImage (..), IndexedImages)

makeToSchema ''IndexedImage

makeToSchema ''IndexedImages

data CreateNews = CreateNews
  { _createNews_title :: !Text
  , _createNews_text :: !Text
  , _createNews_category :: Int
  , _createNews_images :: IndexedImages
  }
  deriving (Generic)

processApiRecord ''CreateNews

data EditNews = EditNews
  { _editNews_id :: Int
  , _editNews_text :: !Text
  , _editNews_category :: Int
  , _editNews_images :: IndexedImages
  }
  deriving (Generic)

processApiRecord ''EditNews

makeToSchema ''GetNews
