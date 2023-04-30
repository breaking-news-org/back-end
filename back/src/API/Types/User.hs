{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module API.Types.User (UserName (..), AuthorName (..), UserRegisterForm (..), UserLoginForm (..), AccessToken (..), RefreshToken (..), FullToken (..)) where

import API.TH
import Common.Prelude (Generic, Text, genericToParamSchema)
import Servant.Auth.JWT (FromJWT, ToJWT)
import Service.Types.User (AuthorName (..), CategoryId, ExpiresAt, LoginError, Password, RegisterError, Role, RotateError, SessionId (..), TokenId (..), UserId (..), UserName (..), SomeError)

data UserRegisterForm = UserRegisterForm
  { _userRegisterForm_userName :: !UserName
  -- ^ Private name of a user
  , _userRegisterForm_password :: !Password
  -- ^ User password
  , _userRegisterForm_authorName :: !AuthorName
  -- ^ Public name of a user
  }
  deriving (Show, Eq, Generic)

data UserLoginForm = UserLoginForm
  { _userLoginForm_userName :: !UserName
  -- ^ Private name of a user
  , _userLoginForm_password :: !Password
  -- ^ User password
  }
  deriving (Show, Eq, Generic)

data AccessToken = AccessToken
  { _accessToken_role :: Role
  , _accessToken_expiresAt :: ExpiresAt
  , _accessToken_userId :: UserId
  , _accessToken_id :: TokenId
  -- ^ index within a session
  , _accessToken_sessionId :: SessionId
  -- ^ coincides with the id of the corresponding refresh token
  }
  deriving (Generic)

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

makeSumToSchemaTypes [''UserName, ''AuthorName, ''Password, ''UserId, ''TokenId, ''SessionId, ''ExpiresAt, ''Role, ''CategoryId]
processRecordApiTypes [''UserLoginForm, ''UserRegisterForm, ''AccessToken, ''RefreshToken, ''FullToken]

instance FromJWT AccessToken
instance ToJWT AccessToken

instance FromJWT RefreshToken
instance ToJWT RefreshToken

makeSumToSchemaTypes [''RegisterError, ''LoginError, ''RotateError]

processRecordApiTypes' [[''SomeError, ''LoginError], [''SomeError, ''RegisterError], [''SomeError, ''RotateError]]