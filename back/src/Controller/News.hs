module Controller.News where

import API.Types.News (CreateNews (..), NewsIdHashed)
import Common.Prelude (Eff, type (:>))
import Controller.Effects.News (NewsController (..))
import Controller.Prelude (ExceptT (..), ServerError)
import Controller.Types.News (Filters (..), QueryParams (..), SelectedNews (..))
import Controller.Types.User (AccessToken (..))
import Service.Effects.News
import Service.Prelude (interpret, send)
import Service.Types.News (SelectedCategory, SetIsPublished, UnavailableNews, SelectedCategories)

create :: (NewsController :> es) => AccessToken -> CreateNews -> ExceptT ServerError (Eff es) NewsIdHashed
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

getCategories :: (NewsController :> es) => ExceptT ServerError (Eff es) SelectedCategories
getCategories = ExceptT $ send ControllerGetCategories

runNewsController :: (ServiceNews :> es) => Eff (NewsController : es) a -> Eff es a
runNewsController = interpret $ \_ -> \case
  ControllerCreateNews AccessToken{..} createNews -> do
    pure <$> serviceCreateNews _accessToken_userId createNews
  ControllerSelectedNews AccessToken{..} fs ->
    pure <$> serviceSelectNews _accessToken_userId _accessToken_role fs
  ControllerSetIsPublishedNews AccessToken{..} setIsPublishedNews -> do
    pure <$> serviceSetIsPublished _accessToken_userId _accessToken_role setIsPublishedNews
  ControllerGetCategories ->
    pure <$> serviceGetCategories
