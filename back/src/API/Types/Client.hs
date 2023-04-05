module API.Types.Client where

import API.TH (processApiRecord)
import Common.Prelude (Generic, Text, UTCTime)
import Servant.Auth.JWT (FromJWT, ToJWT)
import Service.Types.User (Role)

data ClientToken = ClientToken
  { _clientToken_role :: Role
  , _clientToken_expiresAt :: UTCTime
  , _clientToken_creator :: Text
  , _id :: Int
  }
  deriving (Generic)

processApiRecord ''ClientToken

deriving instance FromJWT ClientToken
deriving instance ToJWT ClientToken