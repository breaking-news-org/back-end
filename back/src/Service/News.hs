module Service.News where

import Common.Prelude (getCurrentTime)
import Control.Lens (has)
import Effectful (Eff, IOE, MonadIO (liftIO), type (:>))
import Effectful.Dispatch.Dynamic (interpret)
import External.Logger (Logger, logDebug, withLogger)
import Persist.Effects.News (NewsRepo, repoInsertNews, repoSelectNews, repoUpdateIsPublished, repoSelectCategories)
import Persist.Types.News as PersistNews (Filters (..), InsertNews (..))
import Service.Effects.News (ServiceNews (..))
import Service.Types.News (CreateNews (..), CreatedAt (CreatedAt))

runNewsService :: (NewsRepo :> es, Logger :> es, IOE :> es) => Eff (ServiceNews : es) a -> Eff es a
runNewsService =
  interpret $ \_ -> \case
    ServiceCreateNews userId CreateNews{..} -> do
      now <- liftIO getCurrentTime
      repoInsertNews
        InsertNews
          { _insertNews_title = _createNews_title
          , _insertNews_category = _createNews_category
          , _insertNews_createdAt = CreatedAt now
          , _insertNews_authorId = userId
          , _insertNews_text = _createNews_text
          , _insertNews_images = _createNews_images
          , _insertNews_isPublished = False
          }
    ServiceSelectNews userId userRole Filters{..} -> do
      -- Suppose a user's session was deleted just before her request to get news
      --
      news <-
        repoSelectNews
          userId
          Filters
            { _filters_showUnpublished = Just $ has #_RoleAdmin userRole
            , ..
            }
      withLogger $ logDebug "Got news list"
      pure news
    ServiceSetIsPublished userId userRole setIsPublishedNews ->
      repoUpdateIsPublished userId userRole setIsPublishedNews
    ServiceGetCategories ->
      repoSelectCategories
