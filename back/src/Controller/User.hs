module Controller.User where

import API.Types.User (AccessToken (..), FullToken (FullToken, _fullToken_accessToken, _fullToken_refreshToken), RefreshToken (..), UserLoginForm (..), UserRegisterForm (..))
import Common.Prelude (NominalDiffTime, addUTCTime, getCurrentTime)
import Controller.Effects.Users (UserController (..))
import Controller.Prelude (ExceptT (..), MonadIO (liftIO), ServerError)
import Controller.Types.User (JWKSettings (..))
import Crypto.JOSE (JWK)
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LT
import Effectful (Eff, IOE, type (:>))
import Effectful.Dispatch.Dynamic (interpret, send)
import Servant.Auth.Server (defaultJWTSettings, makeJWT)
import Service.Types.User (ExpiresAt (ExpiresAt), LoginError (UserDoesNotExist), RegisterError (UserExists), RotateError, SessionId, TokenId, User (..), UserLoginData (..), UserRegisterData (..))
import Service.User (UserService, serviceRotateRefreshToken)
import Service.User qualified as UserService

register :: UserController :> es => JWKSettings -> UserRegisterForm -> ExceptT ServerError (Eff es) (Either RegisterError FullToken)
register jwkSettings = ExceptT . send . ControllerRegisterUser jwkSettings

login :: UserController :> es => JWKSettings -> UserLoginForm -> ExceptT ServerError (Eff es) (Either LoginError FullToken)
login jwkSettings = ExceptT . send . ControllerLoginUser jwkSettings

rotateRefreshToken :: UserController :> es => JWKSettings -> RefreshToken -> ExceptT ServerError (Eff es) (Either RotateError FullToken)
rotateRefreshToken jwkSettings = ExceptT . send . ControllerRotateRefreshToken jwkSettings

runUserController :: (UserService :> es, IOE :> es) => Eff (UserController : es) a -> Eff es a
runUserController = interpret $ \_ -> \case
  ControllerRegisterUser jwkSettings UserRegisterForm{..} -> do
    res <-
      UserService.serviceRegister
        UserRegisterData
          { _userRegisterData_userName = _userRegisterForm_userName
          , _userRegisterData_password = _userRegisterForm_password
          , _userRegisterData_authorName = _userRegisterForm_authorName
          }
    case res of
      Left UserExists -> pure $ Right (Left UserExists)
      Right user -> do
        fullToken <- getFreshFullToken jwkSettings user
        pure $ Right $ Right fullToken
  ControllerLoginUser jwkSettings UserLoginForm{..} -> do
    res <-
      UserService.serviceLogin
        UserLoginData
          { _userLoginData_userName = _userLoginForm_userName
          , _userLoginData_password = _userLoginForm_password
          }
    case res of
      Left UserDoesNotExist -> pure $ Right $ Left UserDoesNotExist
      Right userId -> do
        fullToken <- getFreshFullToken jwkSettings userId
        pure $ Right $ Right fullToken
  ControllerRotateRefreshToken JWKSettings{..} RefreshToken{..} -> do
    expiresAt <- getExpiresAt _jwkSettings_jwtLifetimeSeconds
    tokenIdUser <- serviceRotateRefreshToken expiresAt _refreshToken_sessionId _refreshToken_id
    case tokenIdUser of
      Left x -> pure $ Right $ Left x
      Right (tokenId, user) -> Right . Right <$> makeFullToken _jwkSettings_jwk _refreshToken_sessionId tokenId expiresAt user

-- Right <$> getRotatedFullToken jwkSettings refreshToken

-- TODO separate thread ban expired in database
-- TODO go to database-
-- TODO store refresh tokens in database
-- TODO ask about refresh tokens
getFreshFullToken :: (UserService :> es, IOE :> es) => JWKSettings -> User -> Eff es FullToken
getFreshFullToken settings user = do
  expiresAt <- getExpiresAt settings._jwkSettings_jwtLifetimeSeconds
  sessionId <- UserService.serviceCreateSession expiresAt user._user_id
  makeFullToken settings._jwkSettings_jwk sessionId 0 expiresAt user

getExpiresAt :: MonadIO m => NominalDiffTime -> m ExpiresAt
getExpiresAt lifetime = liftIO getCurrentTime >>= \now -> pure (ExpiresAt $ addUTCTime lifetime now)

makeFullToken :: (IOE :> es) => JWK -> SessionId -> TokenId -> ExpiresAt -> User -> Eff es FullToken
makeFullToken jwkSettings sessionId tokenId expiresAt user = do
  let accessToken =
        AccessToken
          { _accessToken_role = user._user_role
          , _accessToken_expiresAt = expiresAt
          , _accessToken_userId = user._user_id
          , _accessToken_sessionId = sessionId
          , _accessToken_id = tokenId
          }
      refreshToken =
        RefreshToken
          { _refreshToken_expiresAt = expiresAt
          , _refreshToken_sessionId = sessionId
          , _refreshToken_id = tokenId
          }
  accessTokenJWT <-
    liftIO $
      makeJWT accessToken (defaultJWTSettings jwkSettings) Nothing >>= \case
        Left e -> error (show e)
        Right v -> pure v
  refreshTokenJWT <-
    liftIO $
      makeJWT refreshToken (defaultJWTSettings jwkSettings) Nothing >>= \case
        Left e -> error (show e)
        Right v -> pure v
  let
    fullToken =
      FullToken
        { _fullToken_accessToken = LT.toStrict $ LT.decodeUtf8 accessTokenJWT
        , _fullToken_refreshToken = LT.toStrict $ LT.decodeUtf8 refreshTokenJWT
        }
  pure fullToken