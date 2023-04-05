{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Controller.User where

import API.Types.User (UserRegistrationForm (..))
import Common.Prelude (Text)
import Controller.Prelude
import Effectful
import Effectful.Dispatch.Dynamic
import Service.Types.User
import Service.User (UserService)
import Service.User qualified as UserService

data ControllerUser :: Effect where
  Register :: UserRegistrationForm -> ControllerUser m (Either ServerError Text)

type instance DispatchOf ControllerUser = Dynamic

register :: ControllerUser :> es => UserRegistrationForm -> ExceptT ServerError (Eff es) Text
register = ExceptT . send . Register

runUserController ::
  UserService :> es =>
  Eff (ControllerUser : es) a ->
  Eff es a
runUserController = interpret $ \_ -> \case
  Register form -> do
    UserService.serviceRegister
      UserRegistrationData
        { _userRegistrationData_email = form._userRegistrationForm_email
        , _userRegistrationData_password = form._userRegistrationForm_password
        }
    pure $ Right "All OK!"
