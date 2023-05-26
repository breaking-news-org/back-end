module Service.Types.User where

import Common.TH (processRecords)
import Common.Types.User
import Service.Prelude (Generic)

data UserRegisterForm = UserRegisterForm
  { _userName :: UserName
  -- ^ Private name of a user
  , _password :: Password
  -- ^ User password
  , _authorName :: AuthorName
  -- ^ Public name of a user
  }
  deriving (Show, Eq, Generic)

data UserLoginForm = UserLoginForm
  { _userName :: UserName
  -- ^ Private name of a user
  , _password :: Password
  -- ^ User password
  }
  deriving (Show, Eq, Generic)

processRecords [''UserRegisterForm, ''UserLoginForm]