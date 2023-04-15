module Controller.News where

import API.Types.Client (ClientToken (..))
import API.Types.News (CreateNews (..), QueryParams (..))
import API.Types.News qualified as API
import Common.Prelude (Eff, type (:>))
import Controller.Effects.News (NewsController (..))
import Controller.Prelude (ExceptT (..), NoContent (..), ServerError)
import Service.Effects.News qualified as Service (ServiceNews (..), serviceCreateNews, serviceGetNews)
import Service.Prelude (interpret, send)
import Service.Types.News qualified as Service

create :: (NewsController :> es) => ClientToken -> API.CreateNews -> ExceptT ServerError (Eff es) NoContent
create clientToken = ExceptT . send . ControllerCreateNews clientToken

get :: (NewsController :> es) => ClientToken -> QueryParams -> ExceptT ServerError (Eff es) [API.GetNews]
get
  clientToken
  QueryParams
    { _queryParams_createdUntil = _filters_createdUntil
    , _queryParams_createdSince = _filters_createdSince
    , _queryParams_createdAt = _filters_createdAt
    , _queryParams_creator = _filters_creator
    , _queryParams_category = _filters_category
    , _queryParams_content = _filters_content
    , _queryParams_block = _filters_block
    , _queryParams_newsId = _filters_newsId
    } =
    ExceptT $ send $ ControllerGetNews clientToken API.Filters{..}

runNewsController :: (Service.ServiceNews :> es) => Eff (NewsController : es) a -> Eff es a
runNewsController = interpret $ \_ -> \case
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