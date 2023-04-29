module Service.News where

import API.Types.User (AuthorName (..))
import Common.Prelude (getCurrentTime)
import Control.Lens (has, (^.))
import Effectful (Eff, IOE, MonadIO (liftIO), type (:>))
import Effectful.Dispatch.Dynamic (interpret)
import External.Logger (Logger, logDebug, withLogger)
import Persist.Effects.News (NewsRepo, repoInsertNews, repoSelectedNews)
import Persist.Types.News as PersistNews (Filters (..), InsertNews (..), SelectedNews (..))
import Service.Effects.News (ServiceNews (..))
import Service.Types.News as ServiceNews (CreateNews (..), Filters (..), GetNews (..))

runNewsService :: (NewsRepo :> es, Logger :> es, IOE :> es) => Eff (ServiceNews : es) a -> Eff es a
runNewsService =
  interpret $ \_ -> \case
    ServiceCreateNews CreateNews{..} -> do
      now <- liftIO getCurrentTime
      repoInsertNews
        InsertNews
          { _insertNews_title = _createNews_title
          , _insertNews_category = _createNews_category
          , _insertNews_creationDate = now
          , _insertNews_authorName = _createNews_authorName ^. #_AuthorName
          , _insertNews_text = _createNews_text
          , _insertNews_images = _createNews_images
          , _insertNews_isPublished = False
          }
    ServiceGetNews ServiceNews.Filters{..} role -> do
      news <- repoSelectedNews PersistNews.Filters{_filters_showUnpublished = Just $ has #_RoleAdmin role, ..}
      withLogger $ logDebug "Got news list"
      pure $
        ( \SelectedNews{..} ->
            GetNews
              { _getNews_title = _selectNews_title
              , _getNews_creationDate = _selectNews_creationDate
              , _getNews_authorName = AuthorName _selectNews_authorName
              , _getNews_category = _selectNews_category
              , _getNews_text = _selectNews_text
              , _getNews_id = _selectNews_id
              , _getNews_images = _selectNews_images
              , _getNews_isPublished = _selectNews_isPublished
              }
        )
          <$> news