{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Controller.News where

import API.Types.News (filters_createdUntil)
import API.Types.News qualified as API (CreateNews (..), Filters (..), GetNews (..), News)
import Common.Prelude
import Controller.Prelude (ExceptT (..), NoContent (..), ServerError)
import Service.News qualified as Service (NewsService (..), serviceCreateNews, serviceGetNews)
import Service.Prelude (interpret, send)
import Service.Types.News qualified as Service

data NewsController :: Effect where
  ControllerCreateNews :: API.CreateNews -> NewsController m (Either ServerError NoContent)
  -- TODO allow filters
  -- as maybe fields of a record
  ControllerGetNews :: API.Filters Maybe -> NewsController m (Either ServerError [API.News])

type instance DispatchOf NewsController = Dynamic

create :: NewsController :> es => API.CreateNews -> ExceptT ServerError (Eff es) NoContent
create = ExceptT . send . ControllerCreateNews

get :: NewsController :> es => API.Filters Maybe -> ExceptT ServerError (Eff es) [API.News]
get = ExceptT . send . ControllerGetNews

runNewsController :: (Service.NewsService :> es) => Eff (NewsController : es) a -> Eff es a
runNewsController = interpret $ \_ -> \case
  ControllerCreateNews API.CreateNews{..} -> do
    -- take from somewhere
    creator <- undefined
    Service.serviceCreateNews
      ( Service.CreateNews
          { Service._createNews_title
          , Service._createNews_text
          , Service._createNews_category
          , Service._createNews_images
          , Service._createNews_creator = creator
          }
      )
    pure $ Right NoContent
  ControllerGetNews fs -> do
    news <- Service.serviceGetNews fs
    pure $ pure news

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
