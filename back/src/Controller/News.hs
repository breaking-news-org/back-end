{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Controller.News where

import API.Types.News as API (CreateNews (..), GetNews (..))
import Service.News (NewsService (..))
import Service.Prelude (UTCTime)
import Control.Monad.Except
import Effectful
import Effectful.Dispatch.Dynamic
import Servant.API (NoContent (..))
import Servant.Server (ServerError)

data NewsController :: Effect where
  Create :: CreateNews -> NewsController m (Either ServerError NoContent)
  Get :: UTCTime -> NewsController m (Either ServerError [GetNews])

type instance DispatchOf NewsController = Dynamic

-- TODO template this

create :: NewsController :> es => CreateNews -> ExceptT ServerError (Eff es) NoContent
create = ExceptT . send . Create

get :: NewsController :> es => UTCTime -> ExceptT ServerError (Eff es) [GetNews]
get = ExceptT . send . Get

runNewsController ::
  NewsService :> es =>
  Eff (NewsController : es) a ->
  Eff es a
runNewsController = interpret $ \_ -> \case
  Create API.CreateNews{..} -> do
    pure $ Right NoContent
  Get t -> do
    pure $ pure []

-- TODO fix

-- UserService.register _userRegistrationForm_email _userRegistrationForm_password
-- pure $ Right NoContent

-- runUserController ::
--   UserService :> es =>
--   Eff (UserController : es) a ->
--   Eff es a
-- runUserController = interpret $ \_ -> \case
--   Register UserRegistrationForm{..} -> do
--     UserService.register _userRegistrationForm_email _userRegistrationForm_password
--     pure $ Right NoContent
