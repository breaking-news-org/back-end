module Controller.User where

import Common.Prelude (NominalDiffTime, addUTCTime, getCurrentTime, (^?))
import Controller.Effects.User (UserController (..))
import Controller.Prelude
import Controller.Types.User
import Crypto.JOSE (JWK)
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LT
import Effectful (Eff, IOE, type (:>))
import Effectful.Dispatch.Dynamic (interpret, send)
import Servant.Auth.Server (defaultJWTSettings, makeJWT)
import Service.Effects.User (UserService, serviceCreateSession, serviceLogin, serviceRegister, serviceRotateRefreshToken, serviceSetAdmins, serviceUnRegister)
import Service.Types.User

updateAdmins :: UserController :> es => [Admin] -> Eff es ()
updateAdmins = send . ControllerUpdateAdmins

unRegister :: UserController :> es => RefreshToken -> ExceptT ServerError (Eff es) ()
unRegister = ExceptT . send . ControllerUnRegister

register :: UserController :> es => JWKSettings -> UserRegisterForm -> ExceptT ServerError (Eff es) (Either RegisterError FullToken)
register jwkSettings = ExceptT . send . ControllerRegister jwkSettings

login :: UserController :> es => JWKSettings -> UserLoginForm -> ExceptT ServerError (Eff es) (Either RegisteredUserError FullToken)
login jwkSettings = ExceptT . send . ControllerLogin jwkSettings

rotateRefreshToken :: UserController :> es => JWKSettings -> RefreshToken -> ExceptT ServerError (Eff es) (Either RotateError FullToken)
rotateRefreshToken jwkSettings = ExceptT . send . ControllerRotateRefreshToken jwkSettings

runUserController :: (UserService :> es, IOE :> es) => Eff (UserController : es) a -> Eff es a
runUserController = interpret $ \_ -> \case
  ControllerRegister jwkSettings UserRegisterForm{..} -> do
    res <-
      serviceRegister
        UserRegisterForm
          { _userRegisterForm_userName = _userRegisterForm_userName
          , _userRegisterForm_password = _userRegisterForm_password
          , _userRegisterForm_authorName = _userRegisterForm_authorName
          }
    case res of
      Left UserExists -> pure $ Right (Left UserExists)
      Right user -> do
        fullToken <- getFreshFullToken jwkSettings user
        pure $ Right $ Right fullToken
  ControllerLogin jwkSettings UserLoginForm{..} -> do
    res <-
      serviceLogin
        UserLoginForm
          { _userLoginForm_userName = _userLoginForm_userName
          , _userLoginForm_password = _userLoginForm_password
          }
    case res of
      Left err -> pure $ Right $ Left err
      Right userId -> do
        fullToken <- getFreshFullToken jwkSettings userId
        pure $ Right $ Right fullToken
  ControllerRotateRefreshToken JWKSettings{..} refreshToken -> do
    newTokenExpiresAt <- getExpiresAt _jwkSettings_jwtLifetimeSeconds
    tokenIdUser <- serviceRotateRefreshToken refreshToken newTokenExpiresAt
    case tokenIdUser of
      Left x -> pure $ Right $ Left x
      Right (tokenId, user) ->
        Right . Right
          <$> makeFullToken
            _jwkSettings_jwk
            (refreshToken._refreshToken_sessionId, tokenId, newTokenExpiresAt)
            user
  ControllerUpdateAdmins admins -> do
    -- TODO valid?
    serviceSetAdmins admins
  ControllerUnRegister RefreshToken{..} -> do
    pure <$> serviceUnRegister _refreshToken_sessionId

getFreshFullToken :: (UserService :> es, IOE :> es) => JWKSettings -> DBUser -> Eff es FullToken
getFreshFullToken settings user = do
  expiresAt <- getExpiresAt settings._jwkSettings_jwtLifetimeSeconds
  sessionId <- serviceCreateSession expiresAt user._user_id
  makeFullToken
    settings._jwkSettings_jwk
    (sessionId, 0, expiresAt)
    user

getExpiresAt :: MonadIO m => NominalDiffTime -> m ExpiresAt
getExpiresAt lifetime = liftIO getCurrentTime >>= \now -> pure (ExpiresAt $ addUTCTime lifetime now)

-- | Make a 'FullToken'
makeFullToken :: (IOE :> es) => JWK -> (SessionId, TokenId, ExpiresAt) -> DBUser -> Eff es FullToken
makeFullToken jwkSettings (sessionId, tokenId, expiresAt) user = do
  let accessToken =
        AccessToken
          { _accessToken_role = user._user_role
          , _accessToken_userId = user._user_id
          , _accessToken_sessionId = sessionId
          , _accessToken_id = tokenId
          }
      expiresAt' = expiresAt ^? #_ExpiresAt
      refreshToken =
        RefreshToken
          { _refreshToken_id = tokenId
          , _refreshToken_sessionId = sessionId
          }
  accessTokenJWT <-
    liftIO $
      makeJWT accessToken (defaultJWTSettings jwkSettings) expiresAt' >>= \case
        Left e -> error (show e)
        Right v -> pure v
  refreshTokenJWT <-
    liftIO $
      makeJWT refreshToken (defaultJWTSettings jwkSettings) expiresAt' >>= \case
        Left e -> error (show e)
        Right v -> pure v
  let
    fullToken =
      FullToken
        { _fullToken_accessToken = LT.toStrict $ LT.decodeUtf8 accessTokenJWT
        , _fullToken_refreshToken = LT.toStrict $ LT.decodeUtf8 refreshTokenJWT
        }
  pure fullToken
