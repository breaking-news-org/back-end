{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}

module Service.Types.User (
  UserRegisterData (..),
  UserLoginData (..),
  Role (..),
  RegisterError (..),
  LoginError (..),
  InsertUser (..),
  RefreshError(..),
  SelectUser (..),
  Password (..),
  AuthorName (..),
  UserName (..),
  SelectedUser (..),
) where

import Common.Prelude (Text)
import Common.TH (makeFromToJSON, processRecord)
import Persist.Types.User (AuthorName (..), InsertUser (..), SelectUser (..), SelectedUser (..), UserName (..))
import Service.Prelude (Generic)

newtype Password = Password Text
  deriving (Show, Eq, Generic)

processRecord ''Password

data UserRegisterData = UserRegisterData
  { _userRegisterData_userName :: !UserName
  -- ^ Private name of a user
  , _userRegisterData_password :: !Password
  -- ^ User password
  , _userRegisterData_authorName :: !AuthorName
  -- ^ Public name of a user
  }
  deriving (Show, Eq, Generic)

data UserLoginData = UserLoginData
  { _userLoginData_userName :: !UserName
  -- ^ Private name of a user
  , _userLoginData_password :: !Password
  -- ^ User password
  }
  deriving (Show, Eq, Generic)

-- | Role of a user
data Role
  = RoleUser
  | RoleAdmin
  deriving (Generic)

makeFromToJSON ''Role

data RegisterError = UserExists deriving (Generic, Show)
data LoginError = UserDoesNotExist deriving (Generic, Show)
data RefreshError = RefreshError deriving (Generic, Show)