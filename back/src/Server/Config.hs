module Server.Config where

import API.TH (processRecord)
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
  { _db_db :: Text
  , _db_user :: Text
  , _db_password :: Text
  , _db_host :: Text
  , _db_port :: Int
  , _db_dbName :: Text
  , _db_numConns :: Int
  }
  deriving (Show, Generic)

processRecord ''DB

data Web = Web
  { _web_port :: Int
  , _web_pageSize :: Int
  }
  deriving (Show, Generic)

processRecord ''Web

data App = App
  { _app_env :: Text
  , _app_db :: DB
  , _app_web :: Web
  }
  deriving (Show, Generic)

processRecord ''App

data Loader conf :: Effect where
  GetConfig :: (FromJSON conf) => (conf -> a) -> Loader conf m a

makeEffect ''Loader

mkConnStr :: DB -> Text
mkConnStr DB{..} = [i|#{_db_db}://#{_db_user}:#{_db_password}@#{_db_host}:#{_db_port}/#{_db_dbName}|]

runLoader :: forall conf es a. (FromJSON conf, IOE :> es) => String -> Eff (Loader conf : es) a -> Eff es a
runLoader var = reinterpret (loadConfigToReader @conf var) $ \_ -> \case
  GetConfig f -> asks f

loadConfigToReader :: forall conf es a. (FromJSON conf, IOE :> es) => String -> Eff (Reader conf : es) a -> Eff es a
loadConfigToReader varConfPath action = do
  conf <- liftIO $ do
    confFile <- lookupEnv varConfPath
    case confFile of
      Nothing -> error [i|Environment doesn't provide the $#{varConfPath} env variable|]
      Just p -> decodeFileThrow p
  runReader conf action