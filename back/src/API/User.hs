{-# LANGUAGE DeriveGeneric #-}

module API.User where

import API.Prelude
import API.Types.User
import Common.Prelude

type OldAPI = ReqBody '[JSON] UserRegistrationForm :> Post '[JSON] NoContent

newtype API route = API
  { register :: route :- "register" :> ReqBody '[JSON] UserRegistrationForm :> Post '[JSON] NoContent
  }
  deriving (Generic)
