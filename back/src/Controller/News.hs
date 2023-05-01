module Controller.News where

import API.Types.News (CreateNews (..), QueryParams (..))
import API.Types.News qualified as API
import API.Types.User (AccessToken (..))
import Common.Prelude (Eff, type (:>))
import Controller.Effects.News (NewsController (..))
import Controller.Prelude (ExceptT (..), NoContent (..), ServerError)
import Service.Effects.News qualified as Service (ServiceNews (..), serviceCreateNews, serviceGetNews)
import Service.Prelude (interpret, send)
import Service.Types.News qualified as Service

create :: (NewsController :> es) => AccessToken -> API.CreateNews -> ExceptT ServerError (Eff es) NoContent
create clientToken = ExceptT . send . ControllerCreateNews clientToken

get :: (NewsController :> es) => AccessToken -> QueryParams -> ExceptT ServerError (Eff es) [API.GetNews]
get
  clientToken
  QueryParams
    { _queryParams_createdUntil = _filters_createdUntil
    , _queryParams_createdSince = _filters_createdSince
    , _queryParams_createdAt = _filters_createdAt
    , _queryParams_authorName = _filters_authorName
    , _queryParams_category = _filters_category
    , _queryParams_titleLike = _filters_titleLike
    , _queryParams_textLike = _filters_textLike
    , _queryParams_block = _filters_block
    } =
    ExceptT $ send $ ControllerGetNews clientToken API.Filters{_filters_showUnpublished = Nothing, ..}

runNewsController :: (Service.ServiceNews :> es) => Eff (NewsController : es) a -> Eff es a
runNewsController = interpret $ \_ -> \case
  ControllerCreateNews AccessToken{..} CreateNews{..} -> do
    Service.serviceCreateNews
      Service.CreateNews
        { Service._createNews_title
        , Service._createNews_text
        , Service._createNews_category
        , Service._createNews_images
        , Service._createNews_userId = _accessToken_userId
        }
    -- TODO send link to news
    pure $ Right NoContent
  ControllerGetNews AccessToken{..} fs -> do
    news <- Service.serviceGetNews fs _accessToken_role
    pure $ pure news