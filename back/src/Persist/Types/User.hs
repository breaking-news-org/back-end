{-# LANGUAGE InstanceSigs #-}

module Persist.Types.User where

import API.Prelude (Generic, Value)
import Common.Prelude (ByteString, FromJSON, Text, ToJSON, (^.))
import Common.TH (processRecord)
import Data.Aeson.Types (FromJSON (parseJSON), Parser, ToJSON (toJSON), withText)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

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

newtype UserName = UserName Text deriving (Show, Eq, Generic)

processRecord ''UserName

newtype AuthorName = AuthorName Text deriving (Show, Eq, Generic)

processRecord ''AuthorName

data InsertUser = InsertUser
  { _insertUser_userName :: !UserName
  , _insertUser_hashedPassword :: !HashedPassword
  , _insertUser_authorName :: !AuthorName
  }
  deriving (Show, Eq, Generic)

-- Just for debugging
processRecord ''InsertUser

data SelectUser = SelectUser
  { _selectUser_userName :: !UserName
  , _selectUser_hashedPassword :: !HashedPassword
  }
  deriving (Show, Eq, Generic)

data SelectedUser = SelectedUser
  { _selectedUser_userName :: !UserName
  , _selectedUser_hashedPassword :: !HashedPassword
  , _selectedUser_authorName :: !AuthorName
  , _selectedUser_id :: !Int
  }
  deriving (Show, Eq, Generic)

processRecord ''SelectedUser