module Integration.Config where

import API.TH (processRecord)
import Service.Types.User (User (..))
import Common.Prelude

data AppConf = AppConf
  { _appConf_host :: String
  , _appConf_port :: Int
  }
  deriving (Generic)

processRecord ''AppConf

data TestConf = TestConf
  { _testConf_app :: AppConf
  , _testConf_users :: [User]
  }
  deriving (Generic)

processRecord ''TestConf