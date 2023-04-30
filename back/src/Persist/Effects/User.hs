module Persist.Effects.User where

import Effectful
import Effectful.TH
import Persist.Types.User

data UserRepo :: Effect where
  RepoInsertUser :: InsertUser -> UserRepo m User
  RepoSelectUser :: SelectUser -> UserRepo m (Maybe User)
  RepoCreateSession :: ExpiresAt -> UserRepo m SessionId
  RepoSelectSession :: SessionId -> UserRepo m (Maybe Session)

makeEffect ''UserRepo