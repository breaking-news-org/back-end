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
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Database.Esqueleto.Experimental (Entity (..), PersistEntity (Key), SqlExpr, Value (unValue), innerJoin, just, like, limit, offset, on, set, unionAll_, updateCount, withRecursive, (=.), (==.), (||.), type (:&) ((:&)))
import Database.Esqueleto.PostgreSQL.JSON (JSONB (JSONB, unJSONB))
import External.Logger (Logger, withLogger)
import Persist.Effects.News (NewsRepo (..))
import Persist.Model (Categories (..), EntityField (..), News (..), Users (..))
import Persist.Prelude (Eff, IOE, MonadIO (liftIO), SqlBackendPool, from, insert, interpret, mkFromSqlKey, mkSqlKey, mkSqlKeyVal, select, table, val, where_, withConn, (&&.), (<=.), (>=.), type (:>))
import Persist.Types.News as PersistNews (Filters (..), InsertNews (..), NewsIdHashed (NewsIdHashed), SelectedCategory (SelectedCategory, _selectedCategory_id, _selectedCategory_name, _selectedCategory_parent), SelectedNews (..), SetIsPublished (..))
import Persist.Types.User (AuthorName, CreatedSince (CreatedSince), CreatedUntil (CreatedUntil), Role (RoleAdmin))
import Server.Config (App (..), Loader, Web (..), getConfig)

runNewsRepo :: (IOE :> es, Logger :> es, Loader App :> es, SqlBackendPool :> es) => Eff (NewsRepo : es) a -> Eff es a
runNewsRepo = interpret $ \_ -> \case
  RepoInsertNews news -> do
    newsIdHashed <- hashKey <$> withConn (insert $ newsToModel news)
    withLogger $ logDebug $ "Inserted " :# ["news" .= encodeToLazyText news]
    pure newsIdHashed
  RepoSelectNews userId filters -> do
    now <- liftIO getCurrentTime
    app <- getConfig id
    -- TODO add full text search
    news <- withConn $ select do
      categoriesAll <-
        withRecursive
          ( do
              category <- from $ table @Categories
              where_ $
                maybe
                  (val True)
                  (\categoryId -> category.id ==. mkSqlKeyVal categoryId)
                  filters._filters_category
              pure category
          )
          unionAll_
          ( \self -> do
              (categories :& _) <-
                from
                  $ table @Categories
                    `innerJoin` self
                  `on` (\(categories :& categoriesOld) -> categories.parent ==. just categoriesOld.id)
              pure categories
          )
      (news :& users :& _) <-
        from
          $ table @News `innerJoin` table @Users
          `on` (\(news :& users) -> news.authorId ==. users.id)
            `innerJoin` categoriesAll
          `on` (\(news :& _ :& categories) -> news.category ==. categories.id)
      let
        thisYear = UTCTime{utctDay = fromGregorian 2023 1 1, utctDayTime = 0}
        createdUntil = news.createdAt L.^. coerce <=. val (filters L.^. #_filters_createdUntil . coerce . non now)
        createdSince = news.createdAt L.^. coerce >=. val (filters L.^. #_filters_createdSince . coerce . non thisYear)
        mkSql :: Getting (Maybe a) Filters (Maybe a) -> (a -> SqlExpr (Value Bool)) -> SqlExpr (Value Bool)
        mkSql l f = maybe (val True) f (filters L.^. l)
        createdAt = mkSql #_filters_createdAt (\x -> news.createdAt ==. val x)
        authorName = mkSql #_filters_authorName (\x -> users.authorName ==. val x)
        -- seems like esqueleto prevents sql injections
        -- https://github.com/bitemyapp/esqueleto/blob/9a4f7d7c3e56357abb1dd2afe822139882577464/README.md?plain=1#L426
        titleLike = mkSql #_filters_titleLike (\x -> news.title `like` val [i|%#{x}%|])
        textLike = mkSql #_filters_textLike (\x -> news.text' `like` val [i|%#{x}%|])
        showUnpublished =
          mkSql
            #_filters_showUnpublished
            (\x -> news.isPublished ||. val x ||. (news.authorId ==. mkSqlKeyVal userId))
      where_
        ( (createdUntil &&. createdSince &&. createdAt &&. authorName)
            &&. (textLike &&. titleLike &&. showUnpublished)
        )
      let pageSize = app._app_web._web_pageSize
          newsBlock = fromIntegral $ filters L.^. #_filters_block . non 0
      offset $ newsBlock * pageSize
      limit pageSize
      pure (news, users.authorName)
    pure $ newsFromModel <$> news
  RepoUpdateIsPublished userId userRole SetIsPublished{..} -> do
    withConn $ do
      affected <- forM (unhashNewsIdHashed <$> _setIsPublished_news) $ \x ->
        updateCount $ \p -> do
          set p [NewsIsPublished =. val _setIsPublished_isPublished]
          where_
            ( p.id
                ==. val x
                &&. if userRole == RoleAdmin
                  then val True
                  else p.authorId ==. mkSqlKeyVal userId
            )
      let unaffected = zip _setIsPublished_news affected ^.. traversed . filtered ((> 0) . snd) . _1
      pure unaffected
  RepoSelectCategories -> do
    withConn $ do
      categories <- select $ from $ table @Categories
      pure $
        ( \Entity{entityVal = Categories{..}, ..} ->
            SelectedCategory
              { _selectedCategory_id = mkFromSqlKey entityKey
              , _selectedCategory_name = categoriesName
              , _selectedCategory_parent = mkFromSqlKey <$> categoriesParent
              }
        )
          <$> categories

-- TODO use a two-way hash
-- so that hashed value can be unhashed into a news key
hashKey :: Key News -> NewsIdHashed
hashKey = NewsIdHashed . T.pack . show

unhashNewsIdHashed :: NewsIdHashed -> Key News
unhashNewsIdHashed (NewsIdHashed t) = read (T.unpack t)

newsToModel :: InsertNews -> News
newsToModel InsertNews{..} =
  News
    { newsTitle = _insertNews_title
    , newsCreatedAt = _insertNews_createdAt
    , newsAuthorId = mkSqlKey _insertNews_authorId
    , newsCategory = mkSqlKey _insertNews_category
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
    , _selectNews_category = mkFromSqlKey newsCategory
    , _selectNews_images = unJSONB newsImages
    , _selectNews_isPublished = newsIsPublished
    , _selectNews_text = newsText'
    , _selectNews_id = mkFromSqlKey entityKey
    }