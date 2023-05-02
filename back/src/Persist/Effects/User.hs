module Persist.Effects.User where

import Effectful
import Effectful.TH
import Persist.Types.User

data UserRepo :: Effect where
  RepoInsertUser :: InsertUser -> UserRepo m DBUser
  RepoSelectRegisteredUser :: SelectUser -> UserRepo m (Maybe DBUser)
  RepoSelectUserByUserName :: UserName -> UserRepo m (Maybe DBUser)
  RepoCreateSession :: ExpiresAt -> UserId -> UserRepo m SessionId
  RepoSelectSessionById :: SessionId -> UserRepo m (Maybe (Session, DBUser))
  RepoSessionUpdateLastAccessTokenId :: ExpiresAt -> SessionId -> UserRepo m ()

makeEffect ''UserRepo