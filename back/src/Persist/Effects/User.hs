module Persist.Effects.User where

import Effectful
import Effectful.TH
import Persist.Types.User

data UserRepo :: Effect where
  RepoInsertUser :: InsertUser -> UserRepo m User
  RepoSelectRegisteredUser :: SelectUser -> UserRepo m (Maybe User)
  RepoSelectUserByUserName :: UserName -> UserRepo m (Maybe User)
  RepoCreateSession :: ExpiresAt -> UserId -> UserRepo m SessionId
  RepoSelectSessionById :: SessionId -> UserRepo m (Maybe (Session, User))
  RepoSessionUpdateLastAccessTokenId :: ExpiresAt -> SessionId -> UserRepo m ()

makeEffect ''UserRepo