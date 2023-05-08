{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module API.Types.News (
  EditNews (..),
  module Service.Types.News,
) where

import API.Prelude (FromHttpApiData, ToHttpApiData)
import API.TH (deriveNewtypeInstances', makeRecordToSchemaTypes, makeSumToSchemaTypes, processRecordApiTypes)
import API.Types.User ()
import Service.Types.News

makeSumToSchemaTypes [''CreatedAt, ''CreatedSince, ''CreatedUntil, ''NewsText, ''NewsTitle, ''NewsId, ''CategoryId, ''CategoryName, ''InsertNewsError]

makeRecordToSchemaTypes [''NewsItem, ''Image, ''SetIsPublished, ''SelectedCategoryItem]

processRecordApiTypes [''EditNews, ''CreateNews]

deriveNewtypeInstances' [''ToHttpApiData, ''FromHttpApiData] [''CategoryId, ''AuthorName, ''CreatedAt, ''CreatedSince, ''CreatedUntil, ''CategoryName, ''NewsText, ''NewsTitle]