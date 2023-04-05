{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}

module Service.Types.User where

import API.Prelude (Parser, ToSchema, Value)
import Common.Prelude (FromJSON (parseJSON), ToJSON (..), withText)
import Common.TH (processRecord)
import Service.Prelude (ByteString, Generic, Text, decodeUtf8, encodeUtf8)

data Password = Password
  { unPassword :: !ByteString
  }
  deriving (Show, Eq, Generic)

instance FromJSON Password where
  parseJSON :: Value -> Parser Password
  parseJSON = withText "Password" $ \t ->
    pure $ Password $ encodeUtf8 t

instance ToJSON Password where
  toJSON :: Password -> Value
  toJSON = toJSON . decodeUtf8 . unPassword

data User = User
  { _user_email :: !Text
  , _user_hashedPassword :: !Password
  , _user_nickname :: !Text
  }
  deriving (Show, Eq, Generic)

processRecord ''User

data UserRegistrationData = UserRegistrationData
  { _userRegistrationData_email :: !Text
  , _userRegistrationData_password :: !Text
  }

data Role = RoleUser | RoleAdmin deriving (Generic, FromJSON, ToJSON, ToSchema)