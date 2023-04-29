module Controller.Types.User (
  AccessToken (..),
  SessionId (..),
  LastId (..),
) where

import API.Types.User (AccessToken (..))
import Common.Prelude (Generic)

newtype SessionId = SessionId Int deriving (Generic)
newtype LastId = LastId Int deriving (Generic)