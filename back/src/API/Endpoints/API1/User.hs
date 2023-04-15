module API.Endpoints.API1.User where

import API.Prelude
import API.Types.User
import Common.Prelude (Text)

newtype API route = API
  { register :: route :- "authorize" :> ReqBody '[JSON] UserRegistrationForm :> Post '[JSON] Text
  }
  deriving (Generic)
