module Persist.Effects.User where

import Effectful
import Effectful.TH
import Service.Types.User

data UserRepo :: Effect where
  CreateUser :: User -> UserRepo m ()

makeEffect ''UserRepo