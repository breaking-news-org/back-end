module Integration.Config where

import Common.Prelude
import Common.TH (processRecord)
import Common.Types.User (DBUser (..))

data AppConf = AppConf
  { _appConf_host :: String
  , _appConf_port :: Int
  }
  deriving (Generic)

processRecord ''AppConf

data TestConf = TestConf
  { _testConf_app :: AppConf
  , _testConf_users :: [DBUser]
  }
  deriving (Generic)

processRecord ''TestConf