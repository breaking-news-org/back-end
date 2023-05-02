module API.Types.User (
  RefreshToken (..),
  FullToken (..),
  module Service.Types.User,
) where

import API.TH
import Common.Prelude (Generic, Text)
import Servant.Auth.JWT (FromJWT, ToJWT)
import Service.Types.User (AccessToken (..), AuthorName (..), CategoryId (..), ExpiresAt (..), LoginError (..), Password (..), RegisterError, Role, RotateError, SessionId (..), TokenId (..), UserId (..), UserLoginForm (..), UserName (..), UserRegisterForm (..))

data RefreshToken = RefreshToken
  { _refreshToken_expiresAt :: ExpiresAt
  -- ^ when the token expires
  , _refreshToken_sessionId :: SessionId
  -- ^ id of a session starting from registration or login
  , _refreshToken_id :: TokenId
  -- ^ index within that session
  }
  deriving (Generic)

data FullToken = FullToken
  { _fullToken_refreshToken :: Text
  , _fullToken_accessToken :: Text
  }
  deriving (Generic, Show)

makeSumToSchemaTypes [''UserName, ''AuthorName, ''Password, ''UserId, ''TokenId, ''SessionId, ''ExpiresAt, ''Role]
processRecordApiTypes [''UserLoginForm, ''UserRegisterForm, ''AccessToken, ''RefreshToken, ''FullToken]

deriveNewtypeInstances' [''FromJWT, ''ToJWT] [''AccessToken, ''RefreshToken]

makeSumToSchemaTypes [''RegisterError, ''LoginError, ''RotateError]
