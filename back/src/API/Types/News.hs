{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module API.Types.News (
  CreateNews (..),
  EditNews (..),
  QueryParams (..),
  module Service.Types.News,
) where

import API.Prelude (FromHttpApiData, Generic, ToHttpApiData)
import API.TH (deriveNewtypeInstances', makeRecordToSchemaTypes, makeSumToSchemaTypes, processRecordApiTypes)
import API.Types.User (AuthorName (..), CategoryId (..))
import Common.Prelude (Text)
import Data.Default (Default)
import Service.Types.News (CreateNews (..), CreatedAt (..), CreatedSince (..), CreatedUntil (..), Image (..), Images, NewsIdHashed (..), NewsText (..), NewsTitle (..), SelectedNews (..), SetIsPublished)

data EditNews = EditNews
  { _editNews_id :: Int
  , _editNews_text :: !Text
  , _editNews_category :: Int
  , _editNews_images :: Images
  }
  deriving (Generic)

data QueryParams = QueryParams
  { _queryParams_createdUntil :: Maybe CreatedUntil
  , _queryParams_createdSince :: Maybe CreatedSince
  , _queryParams_createdAt :: Maybe CreatedAt
  , _queryParams_authorName :: Maybe AuthorName
  , _queryParams_category :: Maybe CategoryId
  , _queryParams_titleLike :: Maybe NewsTitle
  , _queryParams_textLike :: Maybe NewsText
  , _queryParams_block :: Maybe Int
  }
  deriving (Generic)

instance Default QueryParams

makeSumToSchemaTypes [''CreatedAt, ''CreatedSince, ''CreatedUntil, ''NewsText, ''NewsTitle, ''NewsIdHashed, ''CategoryId]

makeRecordToSchemaTypes [''SelectedNews, ''Image, ''SetIsPublished]

processRecordApiTypes [''EditNews, ''CreateNews, ''QueryParams]

deriveNewtypeInstances' [''ToHttpApiData, ''FromHttpApiData] [''CategoryId, ''AuthorName, ''CreatedAt, ''CreatedSince, ''CreatedUntil]
