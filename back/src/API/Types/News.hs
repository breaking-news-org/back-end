module API.Types.News where

import API.Prelude
import API.TH
import API.Types.User ()
import Common.Types.News
import Service.Types.News

makeSumToSchemaTypes [''CreatedAt, ''Image, ''CreatedSince, ''CreatedUntil, ''NewsText, ''NewsTitle, ''NewsId, ''CategoryId, ''CategoryName]
makeSumToParamSchemaTypes [''CreatedAt, ''Image, ''CreatedSince, ''CreatedUntil, ''NewsText, ''NewsTitle, ''NewsId, ''CategoryId, ''CategoryName]
makeRecordToSchemaTypes [ ''CreateNews, ''NewsItem, ''SetIsPublished, ''SelectedCategoryItem]
deriveNewtypeInstances' [''ToHttpApiData, ''FromHttpApiData] [''CategoryId, ''CreatedAt, ''CreatedSince, ''CreatedUntil, ''CategoryName, ''NewsText, ''NewsTitle, ''Image]
makeSumToSchemaTypes [''InsertNewsError]
