{-# LANGUAGE FlexibleInstances #-}

module API.Types.User where

import API.TH
import Common.Prelude

-- | Transport
data UserRegistrationForm = UserRegistrationForm
  { _userRegistrationForm_email :: !Text
  , _userRegistrationForm_password :: !Text
  }
  deriving (Show, Eq, Generic)

processApiRecord ''UserRegistrationForm