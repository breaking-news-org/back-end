module API.Types.User where

import API.TH (processApiRecord)
import Common.Prelude (Generic, Text)

-- | Transport
data UserRegistrationForm = UserRegistrationForm
  { _userRegistrationForm_email :: !Text
  , _userRegistrationForm_password :: !Text
  }
  deriving (Show, Eq, Generic)

processApiRecord ''UserRegistrationForm