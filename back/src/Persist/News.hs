module Persist.News (
  runNewsRepo,
)
where

import External.Logger (Logger)
import Model.News (NewsRepo (..))
import Persist.Model (EntityField (..), News (..))
import Persist.Prelude
import Service.Types.News qualified as Service
import Control.Monad (void)

runNewsRepo ::
  (IOE :> es, SqlBackendPool :> es, Logger :> es) =>
  Eff (NewsRepo : es) a ->
  Eff es a
runNewsRepo = interpret $ \_ -> \case
  PutNews news -> void $ withConn $ insert $ newsToModel news
  GetNews time -> do
    s <- withConn $ select do
      news <- from $ table @News
      where_ (news ^. NewsCreationDate <=. val time)
      pure news
    -- TODO fix
    -- News id?
    let t = modelToNews . entityVal <$> s
    pure t

newsToModel :: Service.News -> News
newsToModel news =
  News -- this might be more complex in production code
    { newsTitle = news._news_title
    , newsCreationDate = news._news_creationDate
    , newsCreator = news._news_creator
    , newsCategory = news._news_category
    , newsImages = news._news_images
    , newsIsPublished = news._news_isPublished
    }

modelToNews :: News -> Service.News
-- TODO fix
modelToNews = undefined