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
  Session (..),
  HashedPassword (..),
  SomeError(..)
) where

import Common.Prelude (Text)
import Common.TH (processRecords, processRecords', processSums)
import Persist.Types.User (AuthorName (..), CategoryId (..), ExpiresAt (..), HashedPassword (..), InsertUser (..), Role (..), SelectUser (..), Session (..), SessionId (..), TokenId (..), User (..), UserId (..), UserName (..))
import Service.Prelude (Generic)

newtype Password = Password Text
  deriving (Show, Eq, Generic)

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

data RegisterError = UserExists deriving (Generic, Show)
data LoginError = UserDoesNotExist deriving (Generic, Show)
data RotateError = SessionDoesNotExist | SessionHasNewerAccessTokenId deriving (Generic, Show)

newtype SomeError e = SomeError {_someError_error :: e} deriving (Generic)

processSums [''RegisterError, ''LoginError, ''RotateError]
processRecords' [[''SomeError, ''LoginError], [''SomeError, ''RegisterError], [''SomeError, ''RotateError]]

-- >>> encode $ SomeError UserExists
-- "{\"error\":\"UserExists\"}"

-- TODO better instances for err
processRecords [''Password]
