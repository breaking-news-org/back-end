{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module API.Types.User (UserName (..), AuthorName (..), UserRegisterForm (..), UserLoginForm (..), AccessToken (..), RefreshToken (..), FullToken (..)) where

import API.TH (makeToSchemaTypes, processApiTypes)
import Common.Prelude (Generic, Text)
import Servant.Auth.JWT (FromJWT, ToJWT)
import Service.Types.User (AuthorName (..), ExpiresAt, Password, Role, SessionId (..), TokenId (..), UserId (..), UserName (..))

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
  , _accessToken_tokenId :: TokenId
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
  , _refreshToken_tokenId :: TokenId
  -- ^ index within that session
  }
  deriving (Generic)

data FullToken = FullToken
  { _fullToken_refreshToken :: Text
  , _fullToken_accessToken :: Text
  }
  deriving (Generic)

makeToSchemaTypes [''UserName, ''AuthorName, ''Password, ''UserId, ''TokenId, ''SessionId, ''ExpiresAt, ''Role]
processApiTypes [''UserLoginForm, ''UserRegisterForm, ''AccessToken, ''RefreshToken, ''FullToken]

instance FromJWT AccessToken
instance ToJWT AccessToken

instance FromJWT RefreshToken
instance ToJWT RefreshToken