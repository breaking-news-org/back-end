module API.Endpoints.API1.User where

import API.Prelude
import API.Types.User

newtype API route = API
  { register :: route :- "register" :> ReqBody '[JSON] UserRegistrationForm :> Put '[JSON] NoContent
  }
  deriving (Generic)
