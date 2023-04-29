{-# LANGUAGE DeriveGeneric #-}

module API.Types.User (UserName (..), AuthorName (..), UserRegisterForm (..), UserLoginForm (..), AccessToken (..), RefreshToken (..), FullToken (..)) where

import API.TH (makeToSchema, processApiRecord)
import Common.Prelude (Generic, Text, UTCTime)
import Servant.Auth.JWT (FromJWT, ToJWT)
import Service.Types.User (AuthorName (..), Role, UserName (..), Password)

makeToSchema ''UserName

makeToSchema ''AuthorName

makeToSchema ''Password

data UserRegisterForm = UserRegisterForm
  { _userRegisterForm_userName :: !UserName
  -- ^ Private name of a user
  , _userRegisterForm_password :: !Password
  -- ^ User password
  , _userRegisterForm_authorName :: !AuthorName
  -- ^ Public name of a user
  }
  deriving (Show, Eq, Generic)

processApiRecord ''UserRegisterForm

data UserLoginForm = UserLoginForm
  { _userLoginForm_userName :: !UserName
  -- ^ Private name of a user
  , _userLoginForm_password :: !Password
  -- ^ User password
  }
  deriving (Show, Eq, Generic)

processApiRecord ''UserLoginForm

data AccessToken = AccessToken
  { _accessToken_role :: Role
  , _accessToken_expiresAt :: UTCTime
  , _accessToken_authorName :: !AuthorName
  , _accessToken_tokenId :: Int
  -- ^ index within a session
  , _accessToken_sessionId :: Int
  -- ^ coincides with the id of the corresponding refresh token
  }
  deriving (Generic)

makeToSchema ''Role
processApiRecord ''AccessToken

instance FromJWT AccessToken
instance ToJWT AccessToken

data RefreshToken = RefreshToken
  { _refreshToken_expiresAt :: UTCTime
  -- ^ when the token expires
  , _refeshToken_sessionId :: Int
  -- ^ id of a session starting from registration or login
  , _refreshToken_id :: Int
  -- ^ index within that session
  }
  deriving (Generic)

processApiRecord ''RefreshToken

data FullToken = FullToken
  { _fullToken_refreshToken :: RefreshToken
  , _fullToken_accessToken :: AccessToken
  }
  deriving (Generic)

processApiRecord ''FullToken