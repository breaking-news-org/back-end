module Controller.News where

import API.Types.News (CreateNews (..), NewsIdHashed)
import Common.Prelude (Eff, type (:>))
import Controller.Effects.News (NewsController (..))
import Controller.Prelude (ExceptT (..), NoContent (..), ServerError)
import Controller.Types.News (Filters (..), QueryParams (..), SelectedNews (..))
import Controller.Types.User (AccessToken (..))
import Service.Effects.News
import Service.Prelude (interpret, send)
import Service.Types.News (UnavailableNews, SetIsPublished)

create :: (NewsController :> es) => AccessToken -> CreateNews -> ExceptT ServerError (Eff es) NoContent
create accessToken = ExceptT . send . ControllerCreateNews accessToken

get :: (NewsController :> es) => AccessToken -> QueryParams -> ExceptT ServerError (Eff es) [SelectedNews]
get accessToken QueryParams{..} =
  ExceptT $
    send $
      ControllerSelectedNews
        accessToken
        Filters
          { _filters_showUnpublished = Nothing
          , _filters_createdUntil = _queryParams_createdUntil
          , _filters_createdSince = _queryParams_createdSince
          , _filters_createdAt = _queryParams_createdAt
          , _filters_authorName = _queryParams_authorName
          , _filters_category = _queryParams_category
          , _filters_titleLike = _queryParams_titleLike
          , _filters_textLike = _queryParams_textLike
          , _filters_block = _queryParams_block
          }

setIsPublished :: (NewsController :> es) => AccessToken -> SetIsPublished -> ExceptT ServerError (Eff es) UnavailableNews
setIsPublished accessToken = ExceptT . send . ControllerSetIsPublishedNews accessToken

runNewsController :: (ServiceNews :> es) => Eff (NewsController : es) a -> Eff es a
runNewsController = interpret $ \_ -> \case
  ControllerCreateNews accessToken createNews -> do
    serviceCreateNews accessToken createNews
    -- TODO send link to news
    pure $ Right NoContent
  ControllerSelectedNews accessToken fs ->
    pure <$> serviceSelectNews accessToken fs
  ControllerSetIsPublishedNews accessToken setIsPublishedNews -> do
    pure <$> serviceSetIsPublished accessToken setIsPublishedNews