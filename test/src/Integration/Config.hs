module Integration.Config where

import Common.Prelude
import Common.TH (processRecord)
import Common.Types.User (DBUser (..))

data AppConf = AppConf
  { _host :: String
  , _port :: Int
  }
  deriving (Generic)

processRecord ''AppConf

data TestConf = TestConf
  { _app :: AppConf
  , _users :: [DBUser]
  }
  deriving (Generic)

processRecord ''TestConf