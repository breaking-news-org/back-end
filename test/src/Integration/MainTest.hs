module Integration.MainTest (unit_main) where

import API.API (API)
import API.Prelude (NoContent, type (:<|>) (..))
import API.Types.User (UserRegistrationForm (..))
import Config (getConfig)
import Config qualified
import Control.Exception (catch, throwIO)
import Data.Data (Proxy (..))
import Data.String.Interpolate (i)
import Effectful (runEff)
import GHC.IO.Exception (ExitCode (..))
import Integration.Config
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.Client
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
user :<|> (createNews :<|> getNews) = client api

mkUser :: ClientM NoContent
mkUser =
  user
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
  testConf <- runEff $ Config.runLoader @TestConf _CONFIG_FILE do
    getConfig @TestConf id
  let appConf = testConf._testConf_app
  manager' <- newManager defaultManagerSettings
  res <-
    runClientM
      mkUser
      ( mkClientEnv
          manager'
          (BaseUrl Http appConf._appConf_host appConf._appConf_port "")
      )
  case res of
    Left err -> putStrLn [i|Error: #{err}|]
    Right _ -> print "Ok"