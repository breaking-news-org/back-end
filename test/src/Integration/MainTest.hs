{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Integration.MainTest (unit_main) where

import API.Prelude (NoContent, type (:<|>) ((:<|>)))
import API.Root (API)
import API.Types.News
import API.Types.User (UserRegistrationForm (..))
import Common.Prelude qualified as T
import Control.Exception (catch, throwIO)
import Control.Monad (void)
import Data.ByteString qualified as BS
import Data.Data (Proxy (..))
import Data.Kind (Constraint)
import Data.Sequence (Seq ((:<|)))
import Data.String (IsString)
import Data.String.Interpolate (i)
import Effectful (runEff)
import GHC.Generics (Generic)
import GHC.IO.Exception (ExitCode (..))
import Integration.Config
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Persist.Types.News (IndexedImages (IndexedImages))
import Servant.API ((:>))
import Servant.Auth (Auth, JWT)
import Servant.Client
import Servant.Client.Core (Request, RequestF (..))
import Server.Config
import Service.Prelude (encodeUtf8)
import Service.Types.News (CreateNews (_createNews_category, _createNews_creator))
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

instance (HasJWT auths, HasClient ClientM api) => HasClient ClientM (Auth auths a :> api) where
  type Client ClientM (Auth auths a :> api) = Token -> Client ClientM api

  clientWithRoute :: (HasJWT auths, HasClient ClientM api) => Proxy ClientM -> Proxy (Auth auths a :> api) -> Request -> Client ClientM (Auth auths a :> api)
  clientWithRoute (Proxy :: Proxy ClientM) _ req (Token token) =
    clientWithRoute
      (Proxy :: Proxy ClientM)
      (Proxy :: Proxy api)
      req{requestHeaders = ("Authorization", headerVal) :<| requestHeaders req}
   where
    headerVal = "Bearer " <> token

authorize :<|> withToken = client api

authorizeUser :: ClientM Token
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
  let appConf = testConf._testConf_app
      withClientEnv :: ClientM a -> IO (Either ClientError a)
      withClientEnv f = runClientM f (mkClientEnv manager' (BaseUrl Http appConf._appConf_host appConf._appConf_port ""))
  res <- withClientEnv authorizeUser
  case res of
    Left err -> putStrLn [i|Error: #{err}|]
    Right token -> do
      let createNews :<|> getNews = withToken token
      void $
        withClientEnv $
          createNews
            CreateNews
              { _createNews_title = "title"
              , _createNews_text = "text"
              , _createNews_images = IndexedImages []
              , _createNews_category = 0
              }
              
