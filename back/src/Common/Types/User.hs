module Common.Types.User where

import API.Prelude (FromHttpApiData, Generic, PersistField, ToHttpApiData, UTCTime)
import Common.Prelude (Text)
import Common.TH (processRecords, processSums)
import Data.Aeson (encode)
import Data.String (IsString)
import Data.String.Interpolate (i)
import Database.Esqueleto.Experimental (PersistField (..), PersistFieldSql (sqlType), PersistValue (PersistInt64), SqlType (SqlInt64))
import Servant.Auth.JWT (FromJWT, ToJWT)

newtype Password = Password Text
  deriving (Show, Eq, Generic)
  deriving newtype (IsString)

newtype HashedPassword = HashedPassword Text
  deriving (Generic)
  deriving newtype (PersistField, Eq, Ord, Show, PersistFieldSql, IsString)

newtype UserName = UserName Text
  deriving (Generic)
  deriving newtype (PersistField, Eq, Ord, Show, PersistFieldSql, IsString)

newtype AuthorName = AuthorName Text
  deriving (Generic)
  deriving newtype (PersistField, Eq, Ord, Show, PersistFieldSql, IsString, ToHttpApiData, FromHttpApiData)

newtype UserId = UserId Int
  deriving (Generic)
  deriving newtype (Num, Enum, Show, Eq, Ord, Real, Integral)

-- | Role of a user
data Role
  = RoleUser
  | RoleAdmin
  deriving (Generic, Eq, Ord, Show)

newtype TokenId = TokenId Int
  deriving (Generic)
  deriving newtype (Num, PersistField, Eq, Ord, Show, PersistFieldSql)

newtype SessionId = SessionId Int
  deriving (Generic)
  deriving newtype (Num, Integral, Enum, Real, Ord, Eq)

newtype ExpiresAt = ExpiresAt UTCTime
  deriving (Generic)
  deriving newtype (PersistField, Eq, Ord, Show, PersistFieldSql)

data AccessToken = AccessToken
  { _role :: Role
  , _userId :: UserId
  , _id :: TokenId
  -- ^ index within a session
  , _sessionId :: SessionId
  -- ^ coincides with the id of the corresponding refresh token
  }
  deriving (Generic)

data RefreshToken = RefreshToken
  { _sessionId :: SessionId
  -- ^ id of a session starting from registration or login
  , _id :: TokenId
  -- ^ index within that session
  }
  deriving (Generic)

data InsertUser = InsertUser
  { _userName :: UserName
  , _hashedPassword :: HashedPassword
  , _authorName :: AuthorName
  , _role :: Role
  }
  deriving (Show, Eq, Generic)

data SelectUser = SelectUser
  { _userName :: UserName
  , _password :: Password
  }
  deriving (Show, Eq, Generic)

instance PersistField Role where
  toPersistValue :: Role -> PersistValue
  toPersistValue =
    PersistInt64 . \case
      RoleUser -> 1
      RoleAdmin -> 2
  fromPersistValue :: PersistValue -> Either Text Role
  fromPersistValue v
    | v == toPersistValue RoleUser = Right RoleUser
    | v == toPersistValue RoleAdmin = Right RoleAdmin
    | otherwise = Left [i|Role #{v} is undefined|]

instance PersistFieldSql Role where
  sqlType _ = SqlInt64

data DBUser = DBUser
  { _userName :: UserName
  , _hashedPassword :: HashedPassword
  , _authorName :: AuthorName
  , _role :: Role
  , _id :: UserId
  }
  deriving (Show, Eq, Generic)

data Session = Session
  { _tokenId :: TokenId
  , _tokenExpiresAt :: ExpiresAt
  , _id :: SessionId
  }
  deriving (Generic)

data Admin = Admin
  { _userName :: UserName
  , _password :: Password
  }
  deriving (Show, Generic)

data RegisterError = UserExists deriving (Generic, Show)
data RotateError = SessionDoesNotExist | SessionHasNewerRefreshTokenId deriving (Generic, Show)
data RegisteredUserError = WrongPassword | UserDoesNotExist deriving (Generic, Show)

processSums
  [ ''RegisterError
  , ''RegisteredUserError
  , ''RotateError
  , ''Role
  ]

processRecords
  [ ''Session
  , ''UserName
  , ''HashedPassword
  , ''Admin
  , ''Password
  , ''ExpiresAt
  , ''SessionId
  , ''UserId
  , ''TokenId
  , ''AuthorName
  , ''AccessToken
  , ''RefreshToken
  , ''InsertUser
  , ''SelectUser
  , ''DBUser
  ]

instance ToJWT AccessToken
instance ToJWT RefreshToken
instance FromJWT AccessToken
instance FromJWT RefreshToken

-- Demo encoding

ex1 = encode (Left SessionDoesNotExist :: Either RotateError Int)
ex2 = encode (Left UserExists :: Either RegisterError Int)
ex3 = encode $ AuthorName "authorName"

-- >>> ex1
-- "{\"Left\":\"SessionDoesNotExist\"}"

-- >>> ex2
-- "{\"Left\":\"UserExists\"}"

-- >>> ex3
-- "\"authorName\""