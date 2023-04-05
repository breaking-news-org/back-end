module Service.News where

import Common.Prelude (getCurrentTime)
import Effectful (Eff, IOE, MonadIO (liftIO), type (:>))
import Effectful.Dispatch.Dynamic (interpret)
import External.Logger (Logger, logInfo, withLogger)
import Persist.Effects.News (NewsRepo, repoInsertNews, repoSelectNews)
import Persist.Types.News as PersistNews (Filters (..), InsertNews (..), SelectNews (..))
import Service.Effects.News (ServiceNews (..))
import Service.Types.News as ServiceNews (CreateNews (..), Filters (..), GetNews (..))

runNewsService :: (NewsRepo :> es, Logger :> es, IOE :> es) => Eff (ServiceNews : es) a -> Eff es a
runNewsService =
  interpret $ \_ -> \case
    ServiceCreateNews CreateNews{..} -> do
      now <- liftIO getCurrentTime
      -- TODO where does creator come from?
      repoInsertNews
        InsertNews
          { _insertNews_title = _createNews_title
          , _insertNews_category = _createNews_category
          , _insertNews_creationDate = now
          , _insertNews_creator = _createNews_creator
          , _insertNews_text = _createNews_text
          , _insertNews_images = _createNews_images
          , _insertNews_isPublished = False
          }
    ServiceGetNews ServiceNews.Filters{..} -> do
      news <- repoSelectNews PersistNews.Filters{..}
      withLogger $ logInfo "Got news list"
      pure $
        ( \SelectNews{..} ->
            GetNews
              { _getNews_title = _selectNews_title
              , _getNews_creationDate = _selectNews_creationDate
              , _getNews_creator = _selectNews_creator
              , _getNews_category = _selectNews_category
              , _getNews_text = _selectNews_text
              , _getNews_id = _selectNews_id
              , _getNews_images = _selectNews_images
              , _getNews_isPublished = _selectNews_isPublished
              }
        )
          <$> news