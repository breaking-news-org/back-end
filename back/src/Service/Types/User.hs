{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Service.Types.User (
  UserRegisterForm (..),
  UserLoginForm (..),
  module Persist.Types.User,
) where

import Common.TH (processRecords)
import Persist.Types.User
import Service.Prelude (Generic)

data UserRegisterForm = UserRegisterForm
  { _userRegisterForm_userName :: !UserName
  -- ^ Private name of a user
  , _userRegisterForm_password :: !Password
  -- ^ User password
  , _userRegisterForm_authorName :: !AuthorName
  -- ^ Public name of a user
  }
  deriving (Show, Eq, Generic)

data UserLoginForm = UserLoginForm
  { _userLoginForm_userName :: !UserName
  -- ^ Private name of a user
  , _userLoginForm_password :: !Password
  -- ^ User password
  }
  deriving (Show, Eq, Generic)

processRecords [''Password]
