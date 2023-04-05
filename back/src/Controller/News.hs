module Controller.News where

import API.Types.Client (ClientToken (..))
import API.Types.News (CreateNews (..))
import API.Types.News qualified as API
import Common.Prelude (Eff, Text, UTCTime, type (:>))
import Controller.Effects.News (ControllerNews (..))
import Controller.Prelude (ExceptT (..), NoContent (..), ServerError)
import Service.Effects.News qualified as Service (ServiceNews (..), serviceCreateNews, serviceGetNews)
import Service.Prelude (interpret, send)
import Service.Types.News qualified as Service

create :: (ControllerNews :> es) => ClientToken -> API.CreateNews -> ExceptT ServerError (Eff es) NoContent
create clientToken = ExceptT . send . ControllerCreateNews clientToken

get ::
  (ControllerNews :> es) =>
  ClientToken ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Text ->
  Maybe Int ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  ExceptT ServerError (Eff es) [API.GetNews]
get
  clientToken
  _filters_createdUntil
  _filters_createdSince
  _filters_createdAt
  _filters_creator
  _filters_category
  _filters_content
  _filters_block
  _filters_newsId =
    ExceptT $ send $ ControllerGetNews clientToken API.Filters{..}

ewsControllerNews :: (Service.ServiceNews :> es) => Eff (ControllerNews : es) a -> Eff es a
ewsControllerNews = interpret $ \_ -> \case
  ControllerCreateNews ClientToken{..} CreateNews{..} -> do
    Service.serviceCreateNews
      ( Service.CreateNews
          { Service._createNews_title
          , Service._createNews_text
          , Service._createNews_category
          , Service._createNews_images
          , Service._createNews_creator = _clientToken_creator
          }
      )
    pure $ Right NoContent
  ControllerGetNews _ fs -> do
    -- TODO select based on user's role
    news <- Service.serviceGetNews fs
    pure $ pure news