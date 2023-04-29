module Controller.User where

import API.Types.User (AccessToken (..), RefreshToken (..), UserLoginForm (..), UserRegisterForm (..))
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
import Effectful.Error.Static (Error, tryError)
import Servant.Auth.Server (JWTSettings, defaultJWTSettings, makeJWT)
import Server.Config (App, Loader)
import Service.Types.User (AuthorName (..), LoginError (UserDoesNotExist), RefreshError, RegisterError (UserExists), Role (RoleUser), UserLoginData (..), UserRegisterData (..))
import Service.User (UserService)
import Service.User qualified as UserService

register :: UserController :> es => JWK -> UserRegisterForm -> ExceptT ServerError (Eff es) (Either RegisterError T.Text)
register jwk = ExceptT . send . ControllerRegisterUser jwk

login :: UserController :> es => JWK -> UserLoginForm -> ExceptT ServerError (Eff es) (Either LoginError T.Text)
login jwk = ExceptT . send . ControllerLoginUser jwk

rotateRefreshToken :: UserController :> es => JWK -> RefreshToken -> ExceptT ServerError (Eff es) (Either RefreshError T.Text)
rotateRefreshToken jwk = ExceptT . send . ControllerRotateRefreshToken jwk

runUserController :: (UserService :> es, Error LoginError :> es, Error RegisterError :> es, Loader App :> es, IOE :> es) => Eff (UserController : es) a -> Eff es a
runUserController = interpret $ \_ -> \case
  ControllerRegisterUser jwk UserRegisterForm{..} -> do
    -- TODO check user already exists
    res <-
      tryError @RegisterError $
        UserService.serviceRegister
          UserRegisterData
            { _userRegisterData_userName = _userRegisterForm_userName
            , _userRegisterData_password = _userRegisterForm_password
            , _userRegisterData_authorName = _userRegisterForm_authorName
            }
    case res of
      Left (_, UserExists) -> pure $ Right $ Left UserExists
      Right _ -> do
        fullToken <- liftIO $ giveFullToken (defaultJWTSettings jwk) RoleUser _userRegisterForm_authorName
        pure $ Right $ Right $ LT.toStrict $ LT.decodeUtf8 fullToken
  ControllerLoginUser jwk UserLoginForm{..} -> do
    res <-
      tryError @LoginError $
        UserService.serviceLogin
          UserLoginData
            { _userLoginData_userName = _userLoginForm_userName
            , _userLoginData_password = _userLoginForm_password
            }
    case res of
      Left (_, UserDoesNotExist) -> pure $ Right $ Left UserDoesNotExist
      Right authorName -> do
        token <- liftIO $ giveFullToken (defaultJWTSettings jwk) RoleUser authorName
        pure $ Right $ Right $ LT.toStrict $ LT.decodeUtf8 token
  ControllerRotateRefreshToken jwk token -> do
    -- TODO give full token
    _

-- TODO go to database-
-- TODO ban expired
-- TODO store refresh tokens in database
-- TODO ask about refresh tokens
giveFullToken :: JWTSettings -> Role -> AuthorName -> IO LBS.ByteString
giveFullToken settings role authorName = do
  now <- getCurrentTime
  let c =
        AccessToken
          { _accessToken_role = role
          , _accessToken_expiresAt = now
          , _accessToken_authorName = authorName
          , _accessToken_sessionId = _
          , _accessToken_tokenId = 0
          }
  makeJWT c settings Nothing >>= \case
    Left e -> error (show e)
    Right v -> pure v

giveRotatedRefreshToken :: RefreshToken -> JWTSettings -> Role -> AuthorName -> IO LBS.ByteString
giveRotatedRefreshToken refreshToken settings role authorName = do
  _

giveJWT :: JWTSettings -> Role -> Int -> AuthorName -> IO LBS.ByteString
giveJWT s role _id creator = do
  now <- getCurrentTime
  let c =
        AccessToken
          { _accessToken_role = role
          , _accessToken_expiresAt = now
          , _accessToken_authorName = creator
          , _accessToken_tokenId = _
          , _accessToken_sessionId = _
          }
  makeJWT c s Nothing >>= \case
    Left e -> error (show e)
    Right v -> pure v