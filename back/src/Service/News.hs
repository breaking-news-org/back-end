module Service.News where

import Common.Prelude (getCurrentTime)
import Common.Types.News
import Control.Lens (has)
import Effectful
import Effectful.Dispatch.Dynamic
import External.Logger
import Persist.Effects.News
import Service.Effects.News
import Service.Types.News

runNewsService :: (NewsRepo :> es, Logger :> es, IOE :> es) => Eff (ServiceNews : es) a -> Eff es a
runNewsService =
  interpret $ \_ -> \case
    ServiceCreateNews userId CreateNews{..} -> do
      now <- liftIO getCurrentTime
      repoInsertNewsItem  
        InsertNewsItem
          { _insertNews_title = _createNews_title
          , _insertNews_category = _createNews_category
          , _insertNews_createdAt = CreatedAt now
          , _insertNews_authorId = userId
          , _insertNews_text = _createNews_text
          , _insertNews_images = _createNews_images
          , _insertNews_isPublished = _createNews_isPublished
          }
    ServiceSelectNews userId userRole fs -> do
      news <- repoSelectNews userId userRole fs
      withLogger $ logDebug "Got news list"
      pure news
    ServiceSetIsPublished userId userRole setIsPublishedNews ->
      repoUpdateIsPublished userId userRole setIsPublishedNews
    ServiceGetCategories categoryIds ->
      repoSelectCategoryIds categoryIds
