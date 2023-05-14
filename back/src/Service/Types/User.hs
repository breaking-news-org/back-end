module Service.Types.User where

import Common.TH (processRecords)
import Common.Types.User
import Service.Prelude (Generic)

data UserRegisterForm = UserRegisterForm
  { _userRegisterForm_userName :: UserName
  -- ^ Private name of a user
  , _userRegisterForm_password :: Password
  -- ^ User password
  , _userRegisterForm_authorName :: AuthorName
  -- ^ Public name of a user
  }
  deriving (Show, Eq, Generic)

data UserLoginForm = UserLoginForm
  { _userLoginForm_userName :: UserName
  -- ^ Private name of a user
  , _userLoginForm_password :: Password
  -- ^ User password
  }
  deriving (Show, Eq, Generic)

processRecords [''UserRegisterForm, ''UserLoginForm]