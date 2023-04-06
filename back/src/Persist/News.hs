module Persist.News (
  runNewsRepo,
)
where

import Common.Prelude (Getting, getCurrentTime, non)
import Common.Prelude qualified as Common ((^.))
import Control.Monad (void)
import Database.Esqueleto.Experimental (Entity (..), SqlExpr, fromSqlKey, like, limit, offset, (==.))
import Database.Esqueleto.PostgreSQL.JSON (JSONB (JSONB, unJSONB))
import Persist.Effects.News (NewsRepo (..))
import Persist.Model (EntityField (..), News (..))
import Persist.Prelude
import Persist.Types.News as PersistNews (Filters (..), InsertNews (..), SelectNews (..), filters_block, filters_category, filters_content, filters_createdAt, filters_createdSince, filters_createdUntil, filters_creator)
import Server.Config (App (..), Loader, Web (..), getConfig)

runNewsRepo :: (IOE :> es, Loader App :> es, SqlBackendPool :> es) => Eff (NewsRepo : es) a -> Eff es a
runNewsRepo = interpret $ \_ -> \case
  RepoInsertNews news -> void $ withConn $ insert $ newsToModel news
  RepoSelectNews filters -> do
    now <- liftIO getCurrentTime
    app <- getConfig id
    let
    s <- withConn $ select do
      news <- from $ table @News
      let
        mkCreatedSql lens' f = f (filters Common.^. lens' . non now)
        createdUntil = mkCreatedSql PersistNews.filters_createdUntil (\x -> news ^. NewsCreationDate <=. val x)
        createdSince = mkCreatedSql PersistNews.filters_createdSince (\x -> news ^. NewsCreationDate >=. val x)
        mkSql :: Getting (Maybe a) (Filters Maybe) (Maybe a) -> (a -> SqlExpr (Value Bool)) -> SqlExpr (Value Bool)
        mkSql l f = maybe (val True) f (filters Common.^. l)
        createdAt = mkSql PersistNews.filters_createdAt (\x -> news ^. NewsCreationDate ==. val x)
        creator = mkSql PersistNews.filters_creator (\x -> news ^. NewsCreator ==. val x)
        category = mkSql PersistNews.filters_category (\x -> news ^. NewsCategory ==. val x)
        newsText = mkSql PersistNews.filters_content (\x -> val x `like` news ^. NewsText)
        newsBlock = fromIntegral $ filters Common.^. PersistNews.filters_block . non 0
      where_
        ( createdUntil &&. createdSince &&. createdAt &&. creator &&. category &&. newsText
        )
      let pageSize = app._app_web._web_pageSize
      offset $ newsBlock * pageSize
      limit pageSize
      pure news

    let t = modelToNews <$> s
    pure t

newsToModel :: InsertNews -> News
newsToModel InsertNews{..} =
  News
    { newsTitle = _insertNews_title
    , newsCreationDate = _insertNews_creationDate
    , newsCreator = _insertNews_creator
    , newsCategory = _insertNews_category
    , newsImages = JSONB _insertNews_images
    , newsIsPublished = _insertNews_isPublished
    , newsText = _insertNews_text
    }

modelToNews :: Entity News -> PersistNews.SelectNews
modelToNews Entity{entityKey, entityVal = News{..}} =
  SelectNews
    { _selectNews_title = newsTitle
    , _selectNews_creationDate = newsCreationDate
    , _selectNews_creator = newsCreator
    , _selectNews_category = newsCategory
    , _selectNews_images = unJSONB newsImages
    , _selectNews_isPublished = newsIsPublished
    , _selectNews_text = newsText
    , _selectNews_id = fromIntegral $ fromSqlKey entityKey
    }