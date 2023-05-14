{-# LANGUAGE InstanceSigs #-}

module Server.Config where

import Common.Prelude (typeMismatch)
import Common.TH
import Control.Exception (SomeException, catch)
import Data.Aeson
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Yaml.Aeson (decodeFileThrow)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Static
import Effectful.TH (makeEffect)
import GHC.Generics (Generic)
import System.Environment (lookupEnv)
import Common.Types.User

data DB = DB
  { _db_db :: Text
  , _db_user :: Text
  , _db_password :: Text
  , _db_host :: Text
  , _db_port :: Int
  , _db_dbName :: Text
  , _db_numConns :: Int
  }
  deriving (Show, Generic)

data Web = Web
  { _web_port :: Int
  , _web_pageSize :: Int
  }
  deriving (Show, Generic)

newtype JWTParameters = JWTParameters
  { _jwtParameters_expirationTime :: Integer
  }
  deriving (Show, Generic)

data AppEnvironment = EnvDev | EnvProd deriving (Show, Generic)

instance FromJSON AppEnvironment where
  parseJSON (String s)
    | s == "dev" = pure EnvDev
    | s == "prod" = pure EnvProd
    | otherwise = fail "Could not parse the app environment"
  parseJSON v = typeMismatch "Object" v

instance ToJSON AppEnvironment where
  toJSON :: AppEnvironment -> Value
  toJSON = \case
    EnvDev -> "dev"
    EnvProd -> "prod"

data App = App
  { _app_env :: AppEnvironment
  , _app_db :: DB
  , _app_web :: Web
  , _app_jwtParameters :: JWTParameters
  , _app_admins :: [Admin]
  }
  deriving (Show, Generic)

processRecords [''DB, ''Web, ''JWTParameters, ''App]

data Loader conf :: Effect where
  GetConfig :: (FromJSON conf) => (conf -> a) -> Loader conf m a

makeEffect ''Loader

mkConnStr :: DB -> Text
mkConnStr DB{..} = [i|#{_db_db}://#{_db_user}:#{_db_password}@#{_db_host}:#{_db_port}/#{_db_dbName}|]

runLoader :: forall conf es a. (FromJSON conf, IOE :> es) => String -> Eff (Loader conf : es) a -> Eff es a
runLoader var = reinterpret (loadConfigToReader @conf var) $ \_ -> \case
  GetConfig f -> asks f

loadConfigToReader :: forall conf es a. (FromJSON conf, IOE :> es) => String -> Eff (Reader conf : es) a -> Eff es a
loadConfigToReader envVarConfigPath action = do
  config <- liftIO $ do
    confFile <- lookupEnv envVarConfigPath
    case confFile of
      Nothing -> error [i|Environment doesn't provide the $#{envVarConfigPath} env variable|]
      Just p -> decodeFileThrow p `catch` (\(x :: SomeException) -> error [i|Error decoding #{p}\n\n#{x}|])
  runReader config action