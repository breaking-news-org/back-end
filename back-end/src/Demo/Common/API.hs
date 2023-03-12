module Demo.Common.API where

import Demo.Common.API.User qualified as User
import Servant.API

type API = "api" :> "user" :> User.API
