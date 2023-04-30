{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Service.Types.User (
  UserRegisterData (..),
  UserLoginData (..),
  Role (..),
  RegisterError (..),
  LoginError (..),
  InsertUser (..),
  RotateError (..),
  SelectUser (..),
  Password (..),
  AuthorName (..),
  UserName (..),
  User (..),
  UserId (..),
  SessionId (..),
  TokenId (..),
  CategoryId (..),
  ExpiresAt (..),
) where

import Common.Prelude (Text)
import Common.TH (processType)
import Persist.Types.User (AuthorName (..), CategoryId (..), ExpiresAt (..), InsertUser (..), SelectUser (..), SessionId (..), TokenId (..), User (..), UserId (..), UserName (..))
import Service.Prelude (Generic)

newtype Password = Password Text
  deriving (Show, Eq, Generic)

processType ''Password

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

processType ''Role

data RegisterError = UserExists deriving (Generic, Show)
data LoginError = UserDoesNotExist deriving (Generic, Show)
data RotateError = RotateError deriving (Generic, Show)