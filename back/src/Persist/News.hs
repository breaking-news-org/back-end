module Persist.News (
  runNewsRepo,
)
where

import Common.Prelude (Getting, UTCTime (..), filtered, fromGregorian, getCurrentTime, non, traversed, (^..))
import Control.Lens (_1)
import Control.Lens qualified as L ((^.))
import Control.Monad (forM)
import Control.Monad.Logger.Aeson (Message ((:#)), logDebug, (.=))
import Data.Aeson.Text (encodeToLazyText)
import Data.Coerce (coerce)
import Data.Text qualified as T
import Database.Esqueleto.Experimental (Entity (..), PersistEntity (Key), SqlExpr, Value (unValue), fromSqlKey, innerJoin, like, limit, offset, on, set, toSqlKey, updateCount, (=.), (==.), (||.), type (:&) ((:&)))
import Database.Esqueleto.PostgreSQL.JSON (JSONB (JSONB, unJSONB))
import External.Logger (Logger, withLogger)
import Persist.Effects.News (NewsRepo (..))
import Persist.Model (EntityField (NewsIsPublished), News (..), Users (..))
import Persist.Prelude (Eff, IOE, MonadIO (liftIO), SqlBackendPool, from, insert, interpret, select, table, val, where_, withConn, (&&.), (<=.), (>=.), type (:>))
import Persist.Types.News as PersistNews (Filters (..), InsertNews (..), NewsIdHashed (NewsIdHashed), SelectedNews (..), SetIsPublished (..))
import Persist.Types.User (AccessToken (..), AuthorName, CreatedSince (CreatedSince), CreatedUntil (CreatedUntil), Role (RoleAdmin))
import Server.Config (App (..), Loader, Web (..), getConfig)

runNewsRepo :: (IOE :> es, Logger :> es, Loader App :> es, SqlBackendPool :> es) => Eff (NewsRepo : es) a -> Eff es a
runNewsRepo = interpret $ \_ -> \case
  RepoInsertNews news -> do
    newsIdHashed <- hashKey <$> withConn (insert $ newsToModel news)
    withLogger $ logDebug $ "Inserted " :# ["news" .= encodeToLazyText news]
    pure newsIdHashed
  RepoSelectNews accessToken filters -> do
    now <- liftIO getCurrentTime
    app <- getConfig id
    news <- withConn $ select do
      (news :& users) <-
        from
          $ table @News `innerJoin` table @Users
          `on` (\(news :& users) -> news.authorId ==. users.id)
      let
        thisYear = UTCTime{utctDay = fromGregorian 2023 1 1, utctDayTime = 0}
        createdUntil = news.createdAt L.^. coerce <=. val (filters L.^. #_filters_createdUntil . coerce . non now)
        createdSince = news.createdAt L.^. coerce >=. val (filters L.^. #_filters_createdSince . coerce . non thisYear)
        mkSql :: Getting (Maybe a) Filters (Maybe a) -> (a -> SqlExpr (Value Bool)) -> SqlExpr (Value Bool)
        mkSql l f = maybe (val True) f (filters L.^. l)
        createdAt = mkSql #_filters_createdAt (\x -> news.createdAt ==. val x)
        userId = mkSql #_filters_authorName (\x -> users.authorName ==. val x)
        category = mkSql #_filters_category (\x -> news.category ==. val x)
        newsText = mkSql #_filters_textLike (\x -> val x `like` news.text')
        showUnpublished =
          mkSql
            #_filters_showUnpublished
            ( \x ->
                news.isPublished
                  ||. val x
                  ||. (news.authorId ==. val (toSqlKey (fromIntegral accessToken._accessToken_userId)))
            )
      where_
        ( createdUntil &&. createdSince &&. createdAt &&. userId &&. category &&. newsText &&. showUnpublished
        )
      let pageSize = app._app_web._web_pageSize
          newsBlock = fromIntegral $ filters L.^. #_filters_block . non 0
      offset $ newsBlock * pageSize
      limit pageSize
      pure (news, users.authorName)
    pure $ newsFromModel <$> news
  RepoUpdateIsPublished AccessToken{..} SetIsPublished{..} -> do
    withConn $ do
      affected <- forM (unhashNewsIdHashed <$> _setIsPublished_news) $ \x ->
        updateCount $ \p -> do
          set p [NewsIsPublished =. val _setIsPublished_isPublished]
          where_
            ( p.id
                ==. val x
                &&. if _accessToken_role == RoleAdmin
                  then val True
                  else p.authorId ==. val (toSqlKey (fromIntegral _accessToken_userId))
            )
      let unaffected = zip _setIsPublished_news affected ^.. traversed . filtered ((> 0) . snd) . _1
      pure unaffected

hashKey :: Key News -> NewsIdHashed
hashKey = NewsIdHashed . T.pack . show

unhashNewsIdHashed :: NewsIdHashed -> Key News
unhashNewsIdHashed (NewsIdHashed t) = read (T.unpack t)

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