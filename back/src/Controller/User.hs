module Controller.User where

import API.Types.User (AccessToken (..), FullToken (FullToken, _fullToken_accessToken, _fullToken_refreshToken), RefreshToken (..), UserLoginForm (..), UserRegisterForm (..))
import Common.Prelude (addUTCTime, getCurrentTime)
import Controller.Effects.Users (UserController (..))
import Controller.Prelude (ExceptT (..), MonadIO (liftIO), ServerError)
import Controller.Types.JWK (JWKSettings (..))
import Data.ByteString.Lazy qualified as LBS
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LT
import Effectful (Eff, IOE, type (:>))
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.Error.Static (Error, tryError)
import Servant.Auth.Server (defaultJWTSettings, makeJWT)
import Service.Types.User (AuthorName (..), ExpiresAt (ExpiresAt), LoginError (UserDoesNotExist), RegisterError (UserExists), Role (RoleUser), RotateError, UserId, UserLoginData (..), UserRegisterData (..))
import Service.User (UserService)
import Service.User qualified as UserService

register :: UserController :> es => JWKSettings -> UserRegisterForm -> ExceptT ServerError (Eff es) (Either RegisterError FullToken)
register jwk = ExceptT . send . ControllerRegisterUser jwk

login :: UserController :> es => JWKSettings -> UserLoginForm -> ExceptT ServerError (Eff es) (Either LoginError FullToken)
login jwk = ExceptT . send . ControllerLoginUser jwk

rotateRefreshToken :: UserController :> es => JWKSettings -> RefreshToken -> ExceptT ServerError (Eff es) (Either RotateError FullToken)
rotateRefreshToken jwk = ExceptT . send . ControllerRotateRefreshToken jwk

runUserController :: (UserService :> es, Error LoginError :> es, Error RegisterError :> es, IOE :> es) => Eff (UserController : es) a -> Eff es a
runUserController = interpret $ \_ -> \case
  ControllerRegisterUser jwk UserRegisterForm{..} -> do
    res <-
      UserService.serviceRegister
        UserRegisterData
          { _userRegisterData_userName = _userRegisterForm_userName
          , _userRegisterData_password = _userRegisterForm_password
          , _userRegisterData_authorName = _userRegisterForm_authorName
          }
    case res of
      Left UserExists -> pure $ Right $ Left UserExists
      Right userId -> do
        fullToken <- getFullToken jwk RoleUser userId
        pure $ Right $ Right fullToken
  ControllerLoginUser jwk UserLoginForm{..} -> do
    res <-
      UserService.serviceLogin
        UserLoginData
          { _userLoginData_userName = _userLoginForm_userName
          , _userLoginData_password = _userLoginForm_password
          }
    case res of
      Left UserDoesNotExist -> pure $ Right $ Left UserDoesNotExist
      Right userId -> do
        fullToken <- getFullToken jwk RoleUser userId
        pure $ Right $ Right fullToken
  ControllerRotateRefreshToken jwk token -> do
    -- TODO give full token
    _

-- TODO separate thread ban expired in database

-- TODO go to database-
-- TODO store refresh tokens in database
-- TODO ask about refresh tokens
getFullToken :: (UserService :> es, IOE :> es) => JWKSettings -> Role -> UserId -> Eff es FullToken
getFullToken JWKSettings{..} role userId = do
  now <- liftIO getCurrentTime
  let expiresAt = ExpiresAt $ addUTCTime _jwkSettings_jwtLifetimeSeconds now
  sessionId <- UserService.serviceCreateSession expiresAt
  let accessToken =
        AccessToken
          { _accessToken_role = role
          , _accessToken_expiresAt = expiresAt
          , _accessToken_userId = userId
          , _accessToken_sessionId = sessionId
          , _accessToken_tokenId = 0
          }
      refreshToken =
        RefreshToken
          { _refreshToken_expiresAt = expiresAt
          , _refreshToken_tokenId = 0
          , _refreshToken_sessionId = sessionId
          }
  accessTokenJWT <-
    liftIO $
      makeJWT accessToken (defaultJWTSettings _jwkSettings_jwk) Nothing >>= \case
        Left e -> error (show e)
        Right v -> pure v
  refreshTokenJWT <-
    liftIO $
      makeJWT refreshToken (defaultJWTSettings _jwkSettings_jwk) Nothing >>= \case
        Left e -> error (show e)
        Right v -> pure v
  let
    fullToken =
      FullToken
        { _fullToken_accessToken = LT.toStrict $ LT.decodeUtf8 accessTokenJWT
        , _fullToken_refreshToken = LT.toStrict $ LT.decodeUtf8 refreshTokenJWT
        }
  pure fullToken

getRotatedRefreshToken :: (UserService :> es, IOE :> es) => JWKSettings -> RefreshToken -> Role -> UserId -> Eff es FullToken
getRotatedRefreshToken settings refreshToken role userId = do
  newRefreshToken <- UserService.rotateRefreshToken
  pure _
