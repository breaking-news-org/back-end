module API.Types.User (
  FullToken (..),
) where

import API.Prelude
import API.TH
import Common.Prelude (Text)
import Common.TH (processRecords)
import Common.Types.User
import Servant.Auth.JWT (FromJWT, ToJWT)
import Service.Types.User

data FullToken = FullToken
  { _fullToken_refreshToken :: Text
  , _fullToken_accessToken :: Text
  }
  deriving (Generic, Show)

processRecords [''FullToken]
instance ToJWT FullToken
instance FromJWT FullToken

makeRecordToSchemaTypes
  [ ''UserId
  , ''AuthorName
  , ''UserName
  , ''Password
  , ''UserLoginForm
  , ''UserRegisterForm
  , ''FullToken
  ]

makeRecordToParamSchemaTypes
  [ ''UserId
  , ''AuthorName
  , ''UserName
  , ''Password
  ]

makeSumToSchemaTypes [''RegisterError, ''RegisteredUserError, ''RotateError]