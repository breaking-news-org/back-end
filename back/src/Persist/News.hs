module Persist.News (
  runNewsRepo,
)
where

import Common.Prelude (Getting, UTCTime (..), fromGregorian, getCurrentTime, non)
import Common.Prelude qualified as Common ((^.))
import Control.Monad (void)
import Control.Monad.Logger.Aeson
import Data.Aeson.Text (encodeToLazyText)
import Database.Esqueleto.Experimental (Entity (..), SqlExpr, fromSqlKey, like, limit, offset, (==.))
import Database.Esqueleto.PostgreSQL.JSON (JSONB (JSONB, unJSONB))
import External.Logger (Logger, withLogger)
import Persist.Effects.News (NewsRepo (..))
import Persist.Model (EntityField (..), News (..))
import Persist.Prelude
import Persist.Types.News as PersistNews (Filters (..), InsertNews (..), SelectedNews (..))
import Server.Config (App (..), Loader, Web (..), getConfig)

runNewsRepo :: (IOE :> es, Logger :> es, Loader App :> es, SqlBackendPool :> es) => Eff (NewsRepo : es) a -> Eff es a
runNewsRepo = interpret $ \_ -> \case
  RepoInsertNews news -> do
    void $ withConn $ insert $ newsToModel news
    withLogger $ logDebug $ "Inserted " :# ["news" .= encodeToLazyText news]
  RepoSelectedNews filters -> do
    now <- liftIO getCurrentTime
    app <- getConfig id
    let
    s <- withConn $ select do
      news <- from $ table @News
      let
        thisYear = UTCTime{utctDay = fromGregorian 2023 1 1, utctDayTime = 0}
        createdUntil = news ^. NewsCreationDate <=. val (filters Common.^. #_filters_createdUntil . non now)
        createdSince = news ^. NewsCreationDate >=. val (filters Common.^. #_filters_createdSince . non thisYear)
        mkSql :: Getting (Maybe a) (Filters Maybe) (Maybe a) -> (a -> SqlExpr (Value Bool)) -> SqlExpr (Value Bool)
        mkSql l f = maybe (val True) f (filters Common.^. l)
        createdAt = mkSql #_filters_createdAt (\x -> news ^. NewsCreationDate ==. val x)
        creator = mkSql #_filters_authorName (\x -> news ^. NewsAuthorName ==. val x)
        category = mkSql #_filters_category (\x -> news ^. NewsCategory ==. val x)
        newsText = mkSql #_filters_content (\x -> val x `like` news ^. NewsText)
        showUnpublished = mkSql #_filters_showUnpublished (\x -> news ^. NewsIsPublished ==. val (not x))
      where_
        ( createdUntil &&. createdSince &&. createdAt &&. creator &&. category &&. newsText &&. showUnpublished
        )
      let pageSize = app._app_web._web_pageSize
          newsBlock = fromIntegral $ filters Common.^. #_filters_block . non 0
      offset $ newsBlock * pageSize
      limit pageSize
      pure news
    let t = newsFromModel <$> s
    pure t

newsToModel :: InsertNews -> News
newsToModel InsertNews{..} =
  News
    { newsTitle = _insertNews_title
    , newsCreationDate = _insertNews_creationDate
    , newsAuthorName = _insertNews_authorName
    , newsCategory = _insertNews_category
    , newsImages = JSONB _insertNews_images
    , newsIsPublished = _insertNews_isPublished
    , newsText = _insertNews_text
    }

newsFromModel :: Entity News -> SelectedNews
newsFromModel Entity{entityKey, entityVal = News{..}} =
  SelectedNews
    { _selectNews_title = newsTitle
    , _selectNews_creationDate = newsCreationDate
    , _selectNews_authorName = newsAuthorName
    , _selectNews_category = newsCategory
    , _selectNews_images = unJSONB newsImages
    , _selectNews_isPublished = newsIsPublished
    , _selectNews_text = newsText
    , _selectNews_id = fromIntegral $ fromSqlKey entityKey
    }