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
import API.Types.User (UserRegistrationForm (..))
import Control.Exception (catch, throwIO)
import Control.Monad (void)
import Control.Monad.Free (Free (..))
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
import Persist.Types.News (IndexedImages (IndexedImages))
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
import Service.Prelude (encodeUtf8, (&), (?~))
import Test.Tasty
import Test.Tasty.HUnit

-- https://hackage.haskell.org/package/tasty-1.4.3/docs/Test-Tasty.html#v:defaultMain
unit_main :: IO ()
unit_main =
  defaultMain
    ( testGroup
        "Integration Tests"
        [ userTests
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
  clientWithRoute (Proxy :: Proxy ClientFree) _ req (Token token) =
    clientWithRoute
      (Proxy :: Proxy ClientFree)
      (Proxy :: Proxy api)
      req{requestHeaders = ("Authorization", headerVal) :<| requestHeaders req}
   where
    headerVal = "Bearer " <> token

Routes
  { api1 =
    API.Endpoints.API1.Root.API
      { user = API.Endpoints.API1.User.API{authorize}
      , news
      }
  } = client api

authorizeUser :: ClientFree Token
authorizeUser =
  Token . encodeUtf8
    <$> authorize
      UserRegistrationForm
        { _userRegistrationForm_email = "contact@zelinf.net"
        , _userRegistrationForm_password = "abcdef"
        }

api :: Proxy API
api = Proxy

_CONFIG_FILE :: String
_CONFIG_FILE = "TEST_CONFIG_FILE"

userTests :: TestTree
userTests = testCase "Registration" do
  testConf <- runEff $ runLoader @TestConf _CONFIG_FILE do
    getConfig @TestConf id
  manager' <- newManager defaultManagerSettings
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
              Pure n -> print n >> pure (pure n)
              _ -> error "Error: didn't get a response"
  res <- withClientEnv authorizeUser
  case res of
    Left err -> putStrLn [i|Error: #{err}|]
    Right token -> do
      let API.Endpoints.API1.News.API{get, create} = news token
      void $
        withClientEnv $
          create
            CreateNews
              { _createNews_title = "title"
              , _createNews_text = "text"
              , _createNews_images = IndexedImages []
              , _createNews_category = 0
              }
      ns <- withClientEnv $ get (def & queryParams_category ?~ 1)
      case ns of
        Left err -> error $ show err
        Right ns_ -> BSC.putStrLn $ encode ns_
