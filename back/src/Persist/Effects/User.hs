module Persist.Effects.User where

import Effectful
import Effectful.TH
import Persist.Types.User

data UserRepo :: Effect where
  RepoInsertUser :: InsertUser -> UserRepo m ()
  RepoSelectUser :: SelectUser -> UserRepo m (Maybe SelectedUser)

makeEffect ''UserRepo