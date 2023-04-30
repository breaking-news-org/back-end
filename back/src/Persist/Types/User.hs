{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Persist.Types.User where

import API.Prelude (Generic, PersistField, UTCTime, Value)
import Common.Prelude (ByteString, FromJSON, Text, ToJSON, (^.))
import Common.TH (processTypes)
import Data.Aeson.Types (FromJSON (parseJSON), Parser, ToJSON (toJSON), withText)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Database.Esqueleto.Experimental (PersistFieldSql)

newtype HashedPassword = HashedPassword ByteString
  deriving (Show, Eq, Generic)

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
  deriving newtype (PersistField, Eq, Ord, Show, PersistFieldSql)

newtype AuthorName = AuthorName Text
  deriving (Generic)
  deriving newtype (PersistField, Eq, Ord, Show, PersistFieldSql)

newtype UserId = UserId Int
  deriving (Generic)
  deriving newtype (Num, Enum, Show, Eq, Ord, Real, Integral)

data InsertUser = InsertUser
  { _insertUser_userName :: !UserName
  , _insertUser_hashedPassword :: !HashedPassword
  , _insertUser_authorName :: !AuthorName
  }
  deriving (Show, Eq, Generic)

data SelectUser = SelectUser
  { _selectUser_userName :: !UserName
  , _selectUser_hashedPassword :: !HashedPassword
  }
  deriving (Show, Eq, Generic)

-- TODO
data User = User
  { _dbUser_userName :: !UserName
  , _dbUser_hashedPassword :: !HashedPassword
  , _dbUser_authorName :: !AuthorName
  , _dbUser_id :: !UserId
  }
  deriving (Show, Eq, Generic)

data Session = Session
  { _session_lastAccessTokenId :: TokenId
  , _session_lastAccessTokenExpiresAt :: ExpiresAt
  }

newtype CategoryId = CategoryId Int
  deriving (Generic)
  deriving newtype (Num, PersistField, Eq, Ord, Show, PersistFieldSql)

newtype TokenId = TokenId Int
  deriving (Generic)
  deriving newtype (Num, PersistField, Eq, Ord, Show, PersistFieldSql)

newtype SessionId = SessionId Int
  deriving (Generic)
  deriving newtype (Num)

newtype ExpiresAt = ExpiresAt UTCTime
  deriving (Generic)
  deriving newtype (PersistField, Eq, Ord, Show, PersistFieldSql)

processTypes [''ExpiresAt, ''SessionId, ''User, ''InsertUser, ''UserId, ''AuthorName, ''UserName, ''TokenId, ''CategoryId]