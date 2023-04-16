module Controller.User where

import API.Types.Client (ClientToken (ClientToken), _clientToken_creator, _clientToken_expiresAt, _clientToken_role, _id)
import API.Types.User (UserRegistrationForm (..))
import Common.Prelude (getCurrentTime)
import Controller.Effects.Users (UserController (..))
import Controller.Prelude (ExceptT (..), MonadIO (liftIO), ServerError)
import Crypto.JOSE (JWK)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LT
import Effectful (Eff, IOE, type (:>))
import Effectful.Dispatch.Dynamic (interpret, send)
import Servant.Auth.Server (JWTSettings, defaultJWTSettings, makeJWT)
import Service.Types.User (Role (RoleUser), UserRegistrationData (..))
import Service.User (UserService)
import Service.User qualified as UserService

authorize :: UserController :> es => JWK -> UserRegistrationForm -> ExceptT ServerError (Eff es) T.Text
authorize jwk = ExceptT . send . RegisterController jwk

runUserController ::
  (UserService :> es, IOE :> es) =>
  Eff (UserController : es) a ->
  Eff es a
runUserController = interpret $ \_ -> \case
  RegisterController jwk form -> do
    UserService.serviceRegister
      UserRegistrationData
        { _userRegistrationData_email = form._userRegistrationForm_email
        , _userRegistrationData_password = form._userRegistrationForm_password
        }

    token <- liftIO $ giveJWT (defaultJWTSettings jwk) RoleUser 1 "creator"
    pure $ Right $ LT.toStrict $ LT.decodeUtf8 token

giveJWT :: JWTSettings -> Role -> Int -> T.Text -> IO LBS.ByteString
giveJWT s role _id creator = do
  now <- getCurrentTime
  let c =
        ClientToken
          { _clientToken_role = role
          , _clientToken_expiresAt = now
          , _clientToken_creator = creator
          , _id = 1
          }
  makeJWT c s Nothing >>= \case
    Left e -> error (show e)
    Right v -> pure v