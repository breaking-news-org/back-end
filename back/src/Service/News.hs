module Service.News where

import Common.Prelude (getCurrentTime)
import Common.Types.News
import Effectful
import Effectful.Dispatch.Dynamic
import External.Logger
import Persist.Effects.News
import Service.Effects.News
import Service.Types.News

runNewsService :: (NewsRepo :> es, Logger :> es, IOE :> es) => Eff (ServiceNews : es) a -> Eff es a
runNewsService =
  interpret $ \_ -> \case
    ServiceCreateNews _authorId CreateNews{..} -> do
      now <- liftIO getCurrentTime
      repoInsertNewsItem InsertNewsItem{_createdAt = CreatedAt now, ..}
    ServiceSelectNews userId userRole fs -> do
      news <- repoSelectNews userId userRole fs
      withLogger $ logDebug "Got news list"
      pure news
    ServiceSetIsPublished userId userRole setIsPublishedNews ->
      repoUpdateIsPublished userId userRole setIsPublishedNews
    ServiceGetCategories categoryIds ->
      repoSelectCategoryIds categoryIds
