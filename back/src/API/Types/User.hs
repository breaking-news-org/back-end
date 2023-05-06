module API.Types.User (
  FullToken (..),
  module Service.Types.User,
) where

import API.TH
import Common.Prelude (Generic, Text)
import Servant.Auth.JWT (FromJWT, ToJWT)
import Service.Types.User

data FullToken = FullToken
  { _fullToken_refreshToken :: Text
  , _fullToken_accessToken :: Text
  }
  deriving (Generic, Show)

makeSumToSchemaTypes [''UserName, ''AuthorName, ''Password, ''UserId, ''TokenId, ''SessionId, ''ExpiresAt, ''Role]
processRecordApiTypes [''UserLoginForm, ''UserRegisterForm, ''AccessToken, ''RefreshToken, ''FullToken]

deriveNewtypeInstances' [''FromJWT, ''ToJWT] [''AccessToken, ''RefreshToken]

makeSumToSchemaTypes [''RegisterError, ''RegisteredUserError, ''RotateError]
