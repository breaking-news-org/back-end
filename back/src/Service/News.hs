module Service.News where

import Common.Prelude (getCurrentTime)
import Control.Lens (has)
import Effectful (Eff, IOE, MonadIO (liftIO), type (:>))
import Effectful.Dispatch.Dynamic (interpret)
import External.Logger (Logger, logDebug, withLogger)
import Persist.Effects.News (NewsRepo, repoInsertNews, repoSelectNews, repoUpdateIsPublished)
import Persist.Types.News as PersistNews (Filters (..), InsertNews (..))
import Service.Effects.News (ServiceNews (..))
import Service.Types.News (CreateNews (..), CreatedAt (CreatedAt))
import Service.Types.User (AccessToken (..))

runNewsService :: (NewsRepo :> es, Logger :> es, IOE :> es) => Eff (ServiceNews : es) a -> Eff es a
runNewsService =
  interpret $ \_ -> \case
    ServiceCreateNews accessToken CreateNews{..} -> do
      now <- liftIO getCurrentTime
      repoInsertNews
        InsertNews
          { _insertNews_title = _createNews_title
          , _insertNews_category = _createNews_category
          , _insertNews_createdAt = CreatedAt now
          , _insertNews_authorId = accessToken._accessToken_userId
          , _insertNews_text = _createNews_text
          , _insertNews_images = _createNews_images
          , _insertNews_isPublished = False
          }
    ServiceSelectNews accessToken Filters{..} -> do
      news <-
        repoSelectNews
          accessToken
          Filters
            { _filters_showUnpublished = Just $ has #_RoleAdmin accessToken._accessToken_role
            , ..
            }
      withLogger $ logDebug "Got news list"
      pure news
    -- TODO
    ServiceSetIsPublished accessToken setIsPublishedNews -> 
      repoUpdateIsPublished accessToken setIsPublishedNews
