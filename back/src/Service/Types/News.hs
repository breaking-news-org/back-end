module Service.Types.News (
  CreateNews (..),
  UnavailableNews,
  module Persist.Types.News,
  module Persist.Types.User,
) where

import Common.Prelude (Generic)
import Common.TH (processRecords)
import Persist.Types.News (Filters (..), Image (..), Images, NewsIdHashed (..), NewsText (..), NewsTitle (..), SelectedNews (..), SetIsPublished(..))
import Persist.Types.User (AuthorName (..), CategoryId (..), CreatedAt (..), CreatedSince (..), CreatedUntil (..), UserId (..))

data CreateNews = CreateNews
  { _createNews_title :: !NewsTitle
  , _createNews_text :: !NewsText
  , _createNews_category :: !CategoryId
  , _createNews_images :: !Images
  , _createNews_isPublished :: Bool
  }
  deriving (Generic)

type UnavailableNews = [NewsIdHashed]

processRecords [''SelectedNews]