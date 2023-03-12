module Demo.Backend.Config where

import Control.Lens hiding ((<.>))
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Yaml.Aeson (decodeFileThrow)
import Demo.Common.API.Prelude (aesonOptions)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Static
import Effectful.TH (makeEffect)
import GHC.Generics (Generic)
import System.Environment (lookupEnv)
import System.FilePath

data App = App
  { _app_env :: Text
  , _app_db :: DB
  , _app_web :: Web
  }
  deriving (Show, Generic)

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

newtype Web = Web
  { _web_port :: Int
  }
  deriving (Show, Generic)

makeLenses ''App
makeLenses ''DB
makeLenses ''Web

instance FromJSON App where
  parseJSON = genericParseJSON aesonOptions

instance FromJSON DB where
  parseJSON = genericParseJSON aesonOptions

instance FromJSON Web where
  parseJSON = genericParseJSON aesonOptions

data Loader :: Effect where
  GetConfig :: (App -> a) -> Loader m a

makeEffect ''Loader

mkConnStr :: DB -> Text
mkConnStr DB{..} = [i|#{_db_db}://#{_db_user}:#{_db_password}@#{_db_host}:#{_db_port}/#{_db_dbName}|]

runLoader :: IOE :> es => Eff (Loader : es) a -> Eff es a
runLoader = reinterpret loadConfigToReader $ \_ -> \case
  GetConfig f -> asks f

_CONFIG_FILE :: String
_CONFIG_FILE = "CONFIG_FILE"

loadConfigToReader :: IOE :> es => Eff (Reader App : es) a -> Eff es a
loadConfigToReader action = do
  conf <- liftIO $ do
    confFileEnv <- lookupEnv _CONFIG_FILE
    case confFileEnv of
      Nothing -> error [i|Environment doesn't provide the $#{_CONFIG_FILE} env variable|]
      Just p -> decodeFileThrow p
  runReader conf action