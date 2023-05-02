{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Service.Types.User (
  UserRegisterForm (..),
  UserLoginForm (..),
  RegisterError (..),
  LoginError (..),
  RotateError (..),
  Password (..),
  AccessToken (..),
  module Persist.Types.User,
) where

import Common.Prelude (Text)
import Common.TH (processRecords, processSums)
import Data.Aeson (encode)
import Persist.Types.User (AuthorName (..), CategoryId (..), CreatedAt (..), CreatedSince (..), CreatedUntil (..), DBUser (..), ExpiresAt (..), HashedPassword (..), InsertUser (..), Role (..), SelectUser (..), Session (..), SessionId (..), TokenId (..), UserId (..), UserName (..), AccessToken(..))
import Service.Prelude (Generic)

newtype Password = Password Text
  deriving (Show, Eq, Generic)

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

data RegisterError = UserExists deriving (Generic, Show)
data LoginError = UserDoesNotExist deriving (Generic, Show)
data RotateError = SessionDoesNotExist | SessionHasNewerRefreshTokenId deriving (Generic, Show)

-- TODO better instances for err
processRecords [''Password]

processSums [''RegisterError, ''LoginError, ''RotateError]

-- Demo encoding

ex1 = encode (Left SessionDoesNotExist :: Either RotateError Int)
ex2 = encode (Left UserExists :: Either RegisterError Int)
ex3 = encode $ AuthorName "heul"

-- >>> ex1
-- "{\"Left\":\"SessionDoesNotExist\"}"

-- >>> ex2
-- "{\"Left\":\"UserExists\"}"

-- >>> ex3
-- "\"heu\""
