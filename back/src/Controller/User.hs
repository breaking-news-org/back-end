module Controller.User where

import API.Types.User
import Common.Prelude (NominalDiffTime, addUTCTime, getCurrentTime, (^?))
import Common.Types.User
import Controller.Effects.User (UserController (..))
import Controller.Prelude
import Controller.Types.User
import Crypto.JOSE (JWK)
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LT
import Effectful (Eff, IOE, type (:>))
import Effectful.Dispatch.Dynamic (interpret, send)
import Servant.Auth.Server (defaultJWTSettings, makeJWT)
import Service.Effects.User
import Service.Types.User

updateAdmins :: UserController :> es => [Admin] -> ExceptT ServerError (Eff es) ()
updateAdmins = ExceptT . send . ControllerUpdateAdmins

unregister :: UserController :> es => RefreshToken -> ExceptT ServerError (Eff es) ()
unregister = ExceptT . send . ControllerUnRegister

register :: UserController :> es => JWKSettings -> UserRegisterForm -> ExceptT ServerError (Eff es) (Either RegisterError FullToken)
register jwkSettings = ExceptT . send . ControllerRegister jwkSettings

login :: UserController :> es => JWKSettings -> UserLoginForm -> ExceptT ServerError (Eff es) (Either RegisteredUserError FullToken)
login jwkSettings = ExceptT . send . ControllerLogin jwkSettings

rotateRefreshToken :: UserController :> es => JWKSettings -> RefreshToken -> ExceptT ServerError (Eff es) (Either RotateError FullToken)
rotateRefreshToken jwkSettings = ExceptT . send . ControllerRotateRefreshToken jwkSettings

runUserController :: (UserService :> es, IOE :> es) => Eff (UserController : es) a -> Eff es a
runUserController = interpret $ \_ -> \case
  ControllerRegister jwkSettings form -> do
    res <- serviceRegister form
    case res of
      Left UserExists -> pure $ Right (Left UserExists)
      Right user -> do
        fullToken <- getFreshFullToken jwkSettings user
        pure $ Right $ Right fullToken
  ControllerLogin jwkSettings form -> do
    res <- serviceLogin form
    case res of
      Left err -> pure $ Right $ Left err
      Right userId -> do
        fullToken <- getFreshFullToken jwkSettings userId
        pure $ Right $ Right fullToken
  ControllerRotateRefreshToken JWKSettings{..} refreshToken -> do
    newTokenExpiresAt <- getExpiresAt _jwtLifetimeSeconds
    tokenIdUser <- serviceRotateRefreshToken refreshToken newTokenExpiresAt
    case tokenIdUser of
      Left x -> pure $ Right $ Left x
      Right (tokenId, user) ->
        Right . Right
          <$> makeFullToken
            _jwk
            (refreshToken._sessionId, tokenId, newTokenExpiresAt)
            user
  ControllerUpdateAdmins admins -> do
    -- TODO valid?
    pure <$> serviceUpdateAdmins admins
  ControllerUnRegister RefreshToken{..} -> do
    pure <$> serviceUnRegister _sessionId

getFreshFullToken :: (UserService :> es, IOE :> es) => JWKSettings -> DBUser -> Eff es FullToken
getFreshFullToken settings user = do
  expiresAt <- getExpiresAt settings._jwtLifetimeSeconds
  sessionId <- serviceCreateSession expiresAt user._id
  makeFullToken
    settings._jwk
    (sessionId, 0, expiresAt)
    user

getExpiresAt :: MonadIO m => NominalDiffTime -> m ExpiresAt
getExpiresAt lifetime = liftIO getCurrentTime >>= \now -> pure (ExpiresAt $ addUTCTime lifetime now)

-- | Make a 'FullToken'
makeFullToken :: (IOE :> es) => JWK -> (SessionId, TokenId, ExpiresAt) -> DBUser -> Eff es FullToken
makeFullToken jwkSettings (sessionId, tokenId, expiresAt) user = do
  let accessToken =
        AccessToken
          { _role = user._role
          , _userId = user._id
          , _sessionId = sessionId
          , _id = tokenId
          }
      -- TODO use lens, fix instances
      expiresAt' = expiresAt ^? #_ExpiresAt
      refreshToken =
        RefreshToken
          { _id = tokenId
          , _sessionId = sessionId
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
