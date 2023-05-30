{-# LANGUAGE InstanceSigs #-}

module Server.Config where

import Common.Prelude (typeMismatch)
import Common.TH
import Common.Types.News (CategoryName)
import Common.Types.User
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

data DB = DB
  { _db :: Text
  , _user :: Text
  , _password :: Text
  , _host :: Text
  , _port :: Int
  , _dbName :: Text
  , _numConns :: Int
  }
  deriving (Show, Generic)

data Web = Web
  { _port :: Int
  , _pageSize :: Int
  , _staticContent :: FilePath
  }
  deriving (Show, Generic)

newtype JWTParameters = JWTParameters
  { _expirationTime :: Integer
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

data InitDB = InitDB
  { _admins :: [Admin]
  , _categories :: [CategoryName]
  }
  deriving (Show, Generic)

data App = App
  { _env :: AppEnvironment
  , _dataBase :: DB
  , _web :: Web
  , _jwtParameters :: JWTParameters
  , _initDB :: InitDB
  }
  deriving (Show, Generic)

processRecords [''DB, ''Web, ''JWTParameters, ''InitDB, ''App]

data Loader conf :: Effect where
  GetConfig :: (FromJSON conf) => (conf -> a) -> Loader conf m a

makeEffect ''Loader

mkConnStr :: DB -> Text
mkConnStr DB{..} = [i|#{_db}://#{_user}:#{_password}@#{_host}:#{_port}/#{_dbName}|]

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