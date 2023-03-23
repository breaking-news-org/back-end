{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Controller.User where

import API.Types.User (UserRegistrationForm (..))
import Controller.Prelude
import Service.Types.User
import Service.User (UserService)
import Service.User qualified as UserService
import Effectful
import Effectful.Dispatch.Dynamic
import Servant.API (NoContent (..))
import Servant.Server (ServerError)

data UserController :: Effect where
  Register :: UserRegistrationForm -> UserController m (Either ServerError NoContent)

type instance DispatchOf UserController = Dynamic

register :: UserController :> es => UserRegistrationForm -> ExceptT ServerError (Eff es) NoContent
register = ExceptT . send . Register

runUserController ::
  UserService :> es =>
  Eff (UserController : es) a ->
  Eff es a
runUserController = interpret $ \_ -> \case
  Register form -> do
    UserService.register
      UserRegistrationData
        { email = form._userRegistrationForm_email
        , password = form._userRegistrationForm_password
        }
    pure $ Right NoContent
