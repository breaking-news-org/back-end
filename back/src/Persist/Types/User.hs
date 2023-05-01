{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Persist.Types.User where

import API.Prelude (FromHttpApiData, Generic, PersistField, ToHttpApiData, UTCTime, Value)
import Common.Prelude (ByteString, FromJSON, Text, ToJSON, (^.))
import Common.TH (processRecords, processSums)
import Data.Aeson.Types (FromJSON (parseJSON), Parser, ToJSON (toJSON), withText)
import Data.String (IsString)
import Data.String.Interpolate (i)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Database.Esqueleto.Experimental (PersistField (..), PersistFieldSql (sqlType), PersistValue (PersistInt64), SqlType (SqlInt64))

newtype HashedPassword = HashedPassword ByteString
  deriving (Generic)
  deriving newtype (PersistField, Eq, Ord, Show, PersistFieldSql, IsString)

-- Unsafe because doesn't check if password was hashed
instance FromJSON HashedPassword where
  parseJSON :: Value -> Parser HashedPassword
  parseJSON = withText "HashedPassword" $ \t ->
    pure $ HashedPassword $ encodeUtf8 t

instance ToJSON HashedPassword where
  toJSON :: HashedPassword -> Value
  toJSON = toJSON . decodeUtf8 . (^. #_HashedPassword)

newtype UserName = UserName Text
  deriving (Generic)
  deriving newtype (PersistField, Eq, Ord, Show, PersistFieldSql, IsString)

newtype AuthorName = AuthorName Text
  deriving (Generic)
  deriving newtype (PersistField, Eq, Ord, Show, PersistFieldSql, FromHttpApiData, ToHttpApiData, IsString)

newtype UserId = UserId Int
  deriving (Generic)
  deriving newtype (Num, Enum, Show, Eq, Ord, Real, Integral)

data InsertUser = InsertUser
  { _insertUser_userName :: !UserName
  , _insertUser_hashedPassword :: !HashedPassword
  , _insertUser_authorName :: !AuthorName
  , _insertUser_role :: Role
  }
  deriving (Show, Eq, Generic)

data SelectUser = SelectUser
  { _selectUser_userName :: !UserName
  , _selectUser_hashedPassword :: !HashedPassword
  }
  deriving (Show, Eq, Generic)

-- | Role of a user
data Role
  = RoleUser
  | RoleAdmin
  deriving (Generic, Eq, Ord, Show)

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

-- TODO
data User = User
  { _user_userName :: !UserName
  , _user_hashedPassword :: !HashedPassword
  , _user_authorName :: !AuthorName
  , _user_role :: !Role
  , _user_id :: !UserId
  }
  deriving (Show, Eq, Generic)

data Session = Session
  { _session_lastAccessTokenId :: TokenId
  , _session_lastRefreshTokenExpiresAt :: ExpiresAt
  , _session_id :: SessionId
  }

newtype CategoryId = CategoryId Int
  deriving (Generic)
  deriving newtype (Num, PersistField, Eq, Ord, Show, PersistFieldSql, FromHttpApiData, ToHttpApiData)

newtype TokenId = TokenId Int
  deriving (Generic)
  deriving newtype (Num, PersistField, Eq, Ord, Show, PersistFieldSql)

newtype SessionId = SessionId Int
  deriving (Generic)
  deriving newtype (Num, Integral, Enum, Real, Ord, Eq)

newtype ExpiresAt = ExpiresAt UTCTime
  deriving (Generic)
  deriving newtype (PersistField, Eq, Ord, Show, PersistFieldSql)

newtype CreatedAt = CreatedAt UTCTime
  deriving (Generic)
  deriving newtype (PersistField, Eq, Ord, Show, PersistFieldSql, FromHttpApiData, ToHttpApiData)

newtype CreatedSince = CreatedSince UTCTime
  deriving (Generic)
  deriving newtype (PersistField, Eq, Ord, Show, PersistFieldSql, FromHttpApiData, ToHttpApiData)

newtype CreatedUntil = CreatedUntil UTCTime
  deriving (Generic)
  deriving newtype (PersistField, Eq, Ord, Show, PersistFieldSql, FromHttpApiData, ToHttpApiData)

-- TODO sums or records?
-- processSums
--   []

processRecords
  [ ''User
  , ''ExpiresAt
  , ''SessionId
  , ''UserId
  , ''UserName
  , ''TokenId
  , ''CategoryId
  , ''Role
  , ''CreatedAt
  , ''CreatedUntil
  , ''CreatedSince
  , ''InsertUser
  , ''AuthorName
  ]
