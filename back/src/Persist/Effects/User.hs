module Persist.Effects.User where

import Effectful
import Effectful.TH
import Persist.Types.User

data UserRepo :: Effect where
  RepoInsertUser :: InsertUser -> UserRepo m DBUser
  RepoSelectRegisteredUser :: SelectUser -> UserRepo m (Either RegisteredUserError DBUser)
  RepoSelectUserByUserName :: UserName -> UserRepo m (Maybe DBUser)
  RepoCreateSession :: ExpiresAt -> UserId -> UserRepo m SessionId
  RepoRemoveSessionsByUserId :: UserId -> UserRepo m ()
  RepoSelectSessionById :: SessionId -> UserRepo m (Maybe (Session, DBUser))
  RepoSessionUpdateTokenId :: SessionId -> ExpiresAt -> UserRepo m ()
  RepoUpdateAdmins :: [(UserName, HashedPassword)] -> UserRepo m ()
  RepoRemoveUser :: SessionId -> UserRepo m ()

makeEffect ''UserRepo