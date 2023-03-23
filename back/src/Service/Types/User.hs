module Service.Types.User where

import API.TH (processRecord)
import Service.Prelude (ByteString, Generic, Text, decodeUtf8, encodeUtf8)
import Common.Prelude (FromJSON (parseJSON), ToJSON (..), withText)

data Password = Password
  { unPassword :: !ByteString
  }
  deriving (Show, Eq, Generic)

instance FromJSON Password where
  parseJSON = withText "Password" $ \t ->
    pure $ Password $ encodeUtf8 t

instance ToJSON Password where
  toJSON = toJSON . decodeUtf8 . unPassword

-- | Internal
data User = User
  { _user_email :: !Text
  , _user_hashedPassword :: Password
  , _user_nickname :: !Text
  }
  deriving (Show, Eq, Generic)

processRecord ''User

-- | Internal
data UserRegistrationData = UserRegistrationData
  { email :: Text
  , password :: Text
  }
