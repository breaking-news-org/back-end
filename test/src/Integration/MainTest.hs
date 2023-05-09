{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}

module Integration.MainTest (unit_main) where

import API.Endpoints.API1.News qualified
import API.Endpoints.API1.Root qualified
import API.Endpoints.API1.User qualified
import API.Prelude (encode)
import API.Root (API, Routes (..))
import API.Types.Instances ()
import API.Types.News
import API.Types.User (FullToken (..), UserLoginForm (..), UserRegisterForm (..))
import Common.Prelude (MonadIO (..))
import Control.Exception (catch, throwIO)
import Control.Monad (void)
import Control.Monad.Free (Free (..))
import Controller.User (rotateRefreshToken)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Data (Proxy (..))
import Data.Default (def)
import Data.Kind (Constraint)
import Data.Sequence (Seq ((:<|)))
import Data.String (IsString)
import Data.String.Interpolate (i)
import Effectful (runEff)
import GHC.Generics (Generic)
import GHC.IO.Exception (ExitCode (..))
import Integration.Config
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Client qualified as HTTP
import Servant.API ((:>))
import Servant.Auth (Auth, JWT)
import Servant.Client (BaseUrl (BaseUrl), ClientEnv (..), ClientError, HasClient (..), Scheme (Http), mkClientEnv)
import Servant.Client qualified as I
import Servant.Client.Core (Request, RequestF (..))
import Servant.Client.Free (ClientF (RunRequest, Throw), client)
import Servant.Client.Internal.HttpClient qualified as I
import Servant.Client.Named ()
import Servant.Client.Record ()
import Server.Config
import Service.Prelude (encodeUtf8, (&), (.~), (?~))
import Service.Types.User (Password (..), RegisterError (..), RegisteredUserError (..))
import Test.Tasty
import Test.Tasty.HUnit
import Text.Pretty.Simple (pPrint)

-- https://hackage.haskell.org/package/tasty-1.4.3/docs/Test-Tasty.html#v:defaultMain
unit_main :: IO ()
unit_main =
  defaultMain
    ( testGroup
        "Integration Tests"
        [ authorizeCreateNewsNewsItem
        ]
    )
    `catch` ( \e -> do
                if e == ExitSuccess
                  then putStrLn "Nice"
                  else putStrLn "Bad" >> throwIO e
            )

-- | A compact JWT Token.
newtype Token = Token {getToken :: BS.ByteString}
  deriving (Eq, Show, Read, Generic, IsString)

type family HasJWT xs :: Constraint where
  HasJWT (JWT ': xs) = ()
  HasJWT (x ': xs) = HasJWT xs
  HasJWT '[] = JWTAuthNotEnabled

class JWTAuthNotEnabled

type ClientFree = Free ClientF

instance (HasJWT auths, HasClient ClientFree api) => HasClient ClientFree (Auth auths a :> api) where
  type Client ClientFree (Auth auths a :> api) = Token -> Client ClientFree api

  clientWithRoute :: (HasJWT auths, HasClient ClientFree api) => Proxy ClientFree -> Proxy (Auth auths a :> api) -> Request -> Client ClientFree (Auth auths a :> api)
  clientWithRoute (clientError :: Proxy ClientFree) _ req (Token token) =
    clientWithRoute
      (Proxy :: Proxy ClientFree)
      (Proxy :: Proxy api)
      req{requestHeaders = ("Authorization", headerVal) :<| requestHeaders req}
   where
    headerVal = "Bearer " <> token

Routes
  { api1 =
    API.Endpoints.API1.Root.API
      { user = API.Endpoints.API1.User.API{register, login, unregister, rotateRefreshToken}
      , news
      }
  } = client api

api :: Proxy API
api = Proxy

_CONFIG_FILE :: String
_CONFIG_FILE = "TEST_CONFIG_FILE"

-- TODO create admin in a database on server start
-- server ensures only the admins from the config are in a db

testUserRegisterForm :: UserRegisterForm
testUserRegisterForm =
  UserRegisterForm
    { _userRegisterForm_userName = "testUser1"
    , _userRegisterForm_password = "testPassword"
    , _userRegisterForm_authorName = "testAuthor1"
    }

registerUser :: ClientFree (Either RegisterError FullToken)
registerUser = register testUserRegisterForm

testUserLoginForm :: UserLoginForm
testUserLoginForm =
  UserLoginForm
    { _userLoginForm_userName = testUserRegisterForm._userRegisterForm_userName
    , _userLoginForm_password = testUserRegisterForm._userRegisterForm_password
    }

loginUser :: ClientFree (Either RegisteredUserError FullToken)
loginUser = login testUserLoginForm

-- full workflow
authorizeCreateNewsNewsItem :: TestTree
authorizeCreateNewsNewsItem = testCase "Authorize, create news, get that news" do
  -- get config
  testConf <- runEff $ runLoader @TestConf _CONFIG_FILE do
    getConfig @TestConf id
  manager' <- newManager defaultManagerSettings
  -- run request and be able to show it
  let AppConf{..} = testConf._testConf_app
      withClientEnv :: Show a => ClientFree a -> IO (Either ClientError a)
      withClientEnv f = do
        let env = mkClientEnv manager' (BaseUrl Http _appConf_host _appConf_port "")
        case f of
          Pure a -> pure (pure a)
          Free (Throw err) -> error $ "ERROR: got error right away: " ++ show err
          Free (RunRequest req k) -> do
            let req' = I.defaultMakeClientRequest (baseUrl env) req
            resp <- HTTP.httpLbs req' (manager env)
            putStrLn $ "Got response:\n" <> show resp
            let res = I.clientResponseToResponse id resp
            case k res of
              Pure n -> pPrint n >> pure (pure n)
              _ -> error "Error: didn't get a response"
  res <- withClientEnv registerUser
  tokenMaybe <-
    case res of
      Left clientError -> putStrLn [i|Client error: #{clientError}|] >> pure Nothing
      Right registerResult -> do
        case registerResult of
          Left registerError -> do
            putStrLn [i|Register error: #{registerError}|]
            putStrLn [i|Trying ton login|]
            loginResult <- withClientEnv loginUser
            case loginResult of
              Left clientError -> putStrLn [i|Client error: #{clientError}|] >> pure Nothing
              Right loginResult' ->
                case loginResult' of
                  Left loginError -> putStrLn [i|Login error: #{loginError}|] >> pure Nothing
                  Right fullToken -> pure $ Just fullToken
          Right fullToken -> pure $ Just fullToken
  case tokenMaybe of
    Nothing -> putStrLn [i|Could not obtain a full token|]
    Just token -> do
      let API.Endpoints.API1.News.API{get, create} = news (Token (encodeUtf8 token._fullToken_accessToken))
      void $
        withClientEnv $
          create
            CreateNews
              { _createNews_title = "test_title"
              , _createNews_text = "test_text"
              , _createNews_images = [Image "test_value"]
              , _createNews_category = -1
              , _createNews_isPublished = True
              }
      ns <- withClientEnv $ get def
      case ns of
        Left err -> error $ show err
        Right ns_ -> BSC.putStrLn $ encode ns_
