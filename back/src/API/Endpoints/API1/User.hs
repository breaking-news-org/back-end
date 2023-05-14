module API.Endpoints.API1.User where

import API.Prelude
import API.Types.User
import Common.Types.User
import Servant.Auth
import Service.Types.User

data API route = API
  { register :: route :- "register" :> ReqBody '[JSON] UserRegisterForm :> Post '[JSON] (Either RegisterError FullToken)
  , login :: route :- "login" :> ReqBody '[JSON] UserLoginForm :> Post '[JSON] (Either RegisteredUserError FullToken)
  , unregister :: route :- "unregister" :> Auth '[JWT] RefreshToken :> Get '[JSON] NoContent
  , rotateRefreshToken :: route :- "rotate-refresh-token" :> Auth '[JWT] RefreshToken :> Get '[JSON] (Either RotateError FullToken)
  }
  deriving (Generic)
