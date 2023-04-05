module API.Endpoints.API1.User where

import API.Prelude
import API.Types.User
import Common.Prelude (Text)

newtype API route = API
  { register :: route :- "register" :> ReqBody '[JSON] UserRegistrationForm :> Put '[JSON] Text
  }
  deriving (Generic)
