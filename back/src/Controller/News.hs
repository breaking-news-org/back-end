module Controller.News where

import Common.Prelude (Eff, type (:>))
import Common.Types.News
import Common.Types.User
import Controller.Effects.News (NewsController (..))
import Controller.Prelude (ExceptT (..), ServerError)
import Service.Effects.News
import Service.Prelude (interpret, send)
import Service.Types.News

updateCategories :: NewsController :> es => [CategoryName] -> ExceptT ServerError (Eff es) (Either UpdateCategoriesError ())
updateCategories = ExceptT . send . ControllerUpdateCategories

create :: (NewsController :> es) => AccessToken -> CreateNews -> ExceptT ServerError (Eff es) (Either InsertNewsError ())
create accessToken = ExceptT . send . ControllerCreateNews accessToken

getNews :: (NewsController :> es) => AccessToken -> NewsFilters -> ExceptT ServerError (Eff es) [NewsItem]
getNews accessToken = ExceptT . send . ControllerSelectNews accessToken

setIsPublished :: (NewsController :> es) => AccessToken -> SetIsPublished -> ExceptT ServerError (Eff es) UnavailableNews
setIsPublished accessToken = ExceptT . send . ControllerSetIsPublishedNews accessToken

getCategories :: (NewsController :> es) => CategoryFilters -> ExceptT ServerError (Eff es) SelectedCategories
getCategories = ExceptT . send . ControllerGetCategories

runNewsController :: (ServiceNews :> es) => Eff (NewsController : es) a -> Eff es a
runNewsController = interpret $ \_ -> \case
  ControllerCreateNews AccessToken{..} createNews ->
    pure <$> serviceCreateNews _userId createNews
  ControllerSelectNews AccessToken{..} newsFilters ->
    pure <$> serviceSelectNews _userId _role newsFilters
  ControllerSetIsPublishedNews AccessToken{..} setIsPublishedNews ->
    pure <$> serviceSetIsPublished _userId _role setIsPublishedNews
  ControllerGetCategories categoriesFilters ->
    pure <$> serviceGetCategories categoriesFilters
  ControllerUpdateCategories categories ->
    pure <$> serviceUpdateCategories categories
