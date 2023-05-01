module Persist.News (
  runNewsRepo,
)
where

import Common.Prelude (Getting, UTCTime (..), fromGregorian, getCurrentTime, non)
import Control.Lens ((^.))
import Control.Monad (void)
import Control.Monad.Logger.Aeson (Message ((:#)), logDebug, (.=))
import Data.Aeson.Text (encodeToLazyText)
import Data.Coerce (coerce)
import Database.Esqueleto.Experimental (Entity (..), SqlExpr, Value (unValue), fromSqlKey, innerJoin, like, limit, offset, on, toSqlKey, (==.), type (:&) ((:&)))
import Database.Esqueleto.PostgreSQL.JSON (JSONB (JSONB, unJSONB))
import External.Logger (Logger, withLogger)
import Persist.Effects.News (NewsRepo (..))
import Persist.Model (News (..), Users (..))
import Persist.Prelude (Eff, IOE, MonadIO (liftIO), SqlBackendPool, from, insert, interpret, select, table, val, where_, withConn, (&&.), (<=.), (>=.), type (:>))
import Persist.Types.News as PersistNews (Filters (..), InsertNews (..), SelectedNews (..))
import Persist.Types.User (AuthorName, CreatedSince (CreatedSince), CreatedUntil (CreatedUntil))
import Server.Config (App (..), Loader, Web (..), getConfig)

runNewsRepo :: (IOE :> es, Logger :> es, Loader App :> es, SqlBackendPool :> es) => Eff (NewsRepo : es) a -> Eff es a
runNewsRepo = interpret $ \_ -> \case
  RepoInsertNews news -> do
    void $ withConn $ insert $ newsToModel news
    withLogger $ logDebug $ "Inserted " :# ["news" .= encodeToLazyText news]
  RepoSelectNews filters -> do
    now <- liftIO getCurrentTime
    app <- getConfig id
    news <- withConn $ select do
      (news :& users) <-
        from
          $ table @News `innerJoin` table @Users
          `on` (\(news :& users) -> news.authorId ==. users.id)
      let
        thisYear = UTCTime{utctDay = fromGregorian 2023 1 1, utctDayTime = 0}
        createdUntil = news.createdAt ^. coerce <=. val (filters ^. #_filters_createdUntil . coerce . non now)
        createdSince = news.createdAt ^. coerce >=. val (filters ^. #_filters_createdSince . coerce . non thisYear)
        mkSql :: Getting (Maybe a) (Filters Maybe) (Maybe a) -> (a -> SqlExpr (Value Bool)) -> SqlExpr (Value Bool)
        mkSql l f = maybe (val True) f (filters ^. l)
        createdAt = mkSql #_filters_createdAt (\x -> news.createdAt ==. val x)
        userId = mkSql #_filters_authorName (\x -> users.authorName ==. val x)
        category = mkSql #_filters_category (\x -> news.category ==. val x)
        newsText = mkSql #_filters_textLike (\x -> val x `like` news.text')
        showUnpublished = mkSql #_filters_showUnpublished (\x -> news.isPublished ==. val (not x))
      where_
        ( createdUntil &&. createdSince &&. createdAt &&. userId &&. category &&. newsText &&. showUnpublished
        )
      let pageSize = app._app_web._web_pageSize
          newsBlock = fromIntegral $ filters ^. #_filters_block . non 0
      offset $ newsBlock * pageSize
      limit pageSize
      pure (news, users.authorName)
    pure $ newsFromModel <$> news

-- TODO
-- if a news is unpublished it can only be shown to its author

newsToModel :: InsertNews -> News
newsToModel InsertNews{..} =
  News
    { newsTitle = _insertNews_title
    , newsCreatedAt = _insertNews_createdAt
    , newsAuthorId = toSqlKey $ fromIntegral _insertNews_authorId
    , newsCategory = _insertNews_category
    , newsImages = JSONB _insertNews_images
    , newsIsPublished = _insertNews_isPublished
    , newsText' = _insertNews_text
    }

newsFromModel :: (Entity News, Value AuthorName) -> SelectedNews
newsFromModel (Entity{entityKey, entityVal = News{..}}, authorName) =
  SelectedNews
    { _selectNews_title = newsTitle
    , _selectNews_createdAt = newsCreatedAt
    , _selectNews_authorName = authorName.unValue
    , _selectNews_category = newsCategory
    , _selectNews_images = unJSONB newsImages
    , _selectNews_isPublished = newsIsPublished
    , _selectNews_text = newsText'
    , _selectNews_id = fromIntegral $ fromSqlKey entityKey
    }