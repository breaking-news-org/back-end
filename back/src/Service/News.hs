module Service.News where

import API.Types.News qualified as API (Filters, News (..))
import Common.Prelude (getCurrentTime)
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH
import External.Logger
import Persist.Effects.News (NewsRepo, insertNews, selectNews)
import Service.Types.News qualified as News (CreateNews (..), News (..))

-- TODO
-- if a news is unpublished it can only be shown to its author

data NewsService :: Effect where
  ServiceCreateNews :: News.CreateNews -> NewsService m ()
  ServiceGetNews :: API.Filters Maybe -> NewsService m [API.News]

makeEffect ''NewsService

runNewsService :: (NewsRepo :> es, Logger :> es, IOE :> es) => Eff (NewsService : es) a -> Eff es a
runNewsService =
  interpret $ \_ -> \case
    ServiceCreateNews cn -> do
      now <- liftIO getCurrentTime
      -- TODO Pass around in a TVar
      newsId <- undefined
      -- TODO learn from the context at the call site
      creator <- undefined
      insertNews
        News.News
          { _news_title = cn._createNews_title
          , _news_category = cn._createNews_category
          , _news_creationDate = now
          , _news_creator = creator
          , _news_text = cn._createNews_text
          , _news_id = newsId
          , _news_images = cn._createNews_images
          , _news_isPublished = False
          }
    ServiceGetNews fs -> do
      news <- selectNews fs
      let news' =
            ( \News.News{..} -> API.News{..}
            )
              <$> news
      withLogger $ logInfo "Got news list"
      pure news'