{-# LANGUAGE DeriveGeneric #-}

module API.Types.Client where

import API.TH (makeToSchema, processApiRecord)
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

makeToSchema ''Role
processApiRecord ''ClientToken

instance FromJWT ClientToken

instance ToJWT ClientToken

-- instance ToParamSchema ClientToken