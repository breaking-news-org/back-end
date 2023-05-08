{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Fuse on/on" #-}
module Persist.News (
  runNewsRepo,
)
where

import Common.Prelude (UTCTime (..), filtered, fromGregorian, getCurrentTime, non, traversed, (^..))
import Control.Lens (_1)
import Control.Lens qualified as L ((^.))
import Control.Monad (forM)
import Control.Monad.Logger.Aeson (Message ((:#)), logDebug, (.=))
import Data.Coerce (coerce)
import Data.String.Interpolate (i)
import Database.Esqueleto.Experimental (Entity (..), From, SqlExpr, SqlQuery, Value (..), asc, in_, innerJoin, just, like, limit, notIn, on, orderBy, selectOne, set, unionAll_, updateCount, valList, withRecursive, (=.), (==.), (||.), type (:&) ((:&)))
import Database.Esqueleto.PostgreSQL.JSON (JSONB (JSONB, unJSONB))
import External.Logger (Logger, withLogger)
import Persist.Effects.News (NewsRepo (..))
import Persist.Model (Categories (..), EntityField (..), News (..), Users (..))
import Persist.Prelude (Eff, IOE, MonadIO (liftIO), SqlBackendPool, from, insert, interpret, mkFromSqlKey, mkSqlKey, mkSqlKeyVal, select, table, val, where_, withConn, (&&.), (<=.), (>=.), type (:>))
import Persist.Types.News
import Persist.Types.User
import Server.Config (App (..), Loader, Web (..), getConfig)

-- mkSqlTrue :: s -> Getting (Maybe a) s (Maybe a) -> (a -> SqlExpr (Value Bool)) -> SqlExpr (Value Bool)
mkSqlTrue :: Maybe a -> (a -> SqlExpr (Value Bool)) -> SqlExpr (Value Bool)
mkSqlTrue l f = maybe (val True) f l

runNewsRepo :: (IOE :> es, Logger :> es, Loader App :> es, SqlBackendPool :> es) => Eff (NewsRepo : es) a -> Eff es a
runNewsRepo = interpret $ \_ -> \case
  RepoInsertNewsItem insertNewsItem@InsertNewsItem{..} -> do
    ((unValue <$>) -> authorName) <-
      withConn $
        selectOne do
          users <- from $ table @Users
          where_ (users.id ==. mkSqlKeyVal _insertNews_authorId)
          pure users.name
    case authorName of
      Nothing -> pure . Left $ AuthorDoesNotExist _insertNews_authorId
      Just (UserName authorName') -> do
        let newsModel = newsToModel insertNewsItem
        key <- withConn do
          insert newsModel
        withLogger $ logDebug $ "Inserted " :# ["news" .= insertNewsItem]
        pure . Right $ newsFromModel (Entity{entityKey = key, entityVal = newsModel}, Value (AuthorName authorName'))
  RepoSelectNews userId filters@NewsFilters{..} -> do
    now <- liftIO getCurrentTime
    app <- getConfig id
    news <- withConn $ select do
      categoriesAll <-
        selectCategories
          ( CategoryFilters
              { _categoryFilters_include = _newsFilters_categoriesInclude
              , _categoryFilters_exclude = _newsFilters_categoriesInclude
              }
          )
      (news :& users :& _) <-
        from
          $ table @News `innerJoin` table @Users
          `on` (\(news :& users) -> news.authorId ==. users.id)
            `innerJoin` categoriesAll
          `on` (\(news :& _ :& categories) -> news.category ==. categories.id)
      let
        thisYear = UTCTime{utctDay = fromGregorian 2023 1 1, utctDayTime = 0}
        createdUntil = news.createdAt L.^. coerce <=. val (filters L.^. #_newsFilters_createdUntil . coerce . non now)
        createdSince = news.createdAt L.^. coerce >=. val (filters L.^. #_newsFilters_createdSince . coerce . non thisYear)
        authorName = mkSqlTrue _newsFilters_authorName (\x -> users.authorName ==. val x)
        -- seems like esqueleto prevents sql injections
        -- https://github.com/bitemyapp/esqueleto/blob/9a4f7d7c3e56357abb1dd2afe822139882577464/README.md?plain=1#L426
        -- TODO FULLTEXT search
        titleLike = mkSqlTrue _newsFilters_titleLike (\x -> news.title `like` val [i|%#{x}%|])
        textLike = mkSqlTrue _newsFilters_textLike (\x -> news.text' `like` val [i|%#{x}%|])
        showUnpublished =
          mkSqlTrue
            _newsFilters_showUnpublished
            (\x -> news.isPublished ||. val x ||. (news.authorId ==. mkSqlKeyVal userId))
      where_
        ( (createdUntil &&. createdSince &&. authorName)
            &&. (textLike &&. titleLike &&. showUnpublished)
        )
      let pageSize = fromIntegral app._app_web._web_pageSize
      -- pagination
      limit pageSize
      orderBy [asc news.createdAt]
      pure (news, users.authorName)
    pure $ newsFromModel <$> news
  RepoUpdateIsPublished userId userRole SetIsPublished{..} -> do
    withConn $ do
      affected <- forM _setIsPublished_news $ \x ->
        updateCount $ \p -> do
          set p [NewsIsPublished =. val _setIsPublished_isPublished]
          where_
            ( p.id
                ==. mkSqlKeyVal x
                &&. if userRole == RoleAdmin
                  then val True
                  else p.authorId ==. mkSqlKeyVal userId
            )
      let unaffected = zip _setIsPublished_news affected ^.. traversed . filtered ((> 0) . snd) . _1
      pure unaffected
  RepoSelectCategoryIds categoryFilters -> do
    categories <- withConn $ select $ from =<< selectCategories categoryFilters
    pure $
      ( \Entity{entityVal = Categories{..}, ..} ->
          SelectedCategoryItem
            { _selectedCategory_id = mkFromSqlKey entityKey
            , _selectedCategory_name = categoriesName
            , _selectedCategory_parent = mkFromSqlKey <$> categoriesParent
            }
      )
        <$> categories

-- checkCategoryId ::  SqlExpr (Value (Key Categories)) -> Maybe []

selectCategories :: CategoryFilters -> SqlQuery (From (SqlExpr (Entity Categories)))
selectCategories CategoryFilters{..} =
  withRecursive
    ( do
        category <- from $ table @Categories
        where_ $
          ( case _categoryFilters_include of
              [] -> val True
              x -> category.id `in_` valList (mkSqlKey <$> x)
          )
            &&. ( case _categoryFilters_exclude of
                    [] -> val True
                    x -> category.id `notIn` valList (mkSqlKey <$> x)
                )
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

newsToModel :: InsertNewsItem -> News
newsToModel InsertNewsItem{..} =
  News
    { newsTitle = _insertNews_title
    , newsCreatedAt = _insertNews_createdAt
    , newsAuthorId = mkSqlKey _insertNews_authorId
    , newsCategory = mkSqlKey _insertNews_category
    , newsImages = JSONB _insertNews_images
    , newsIsPublished = _insertNews_isPublished
    , newsText' = _insertNews_text
    }

newsFromModel :: (Entity News, Value AuthorName) -> NewsItem
newsFromModel (Entity{entityKey, entityVal = News{..}}, authorName) =
  NewsItem
    { _newsItem_title = newsTitle
    , _newsItem_createdAt = newsCreatedAt
    , _newsItem_authorName = authorName.unValue
    , _newsItem_category = mkFromSqlKey newsCategory
    , _newsItem_images = unJSONB newsImages
    , _newsItem_isPublished = newsIsPublished
    , _newsItem_text = newsText'
    , _newsItem_id = mkFromSqlKey entityKey
    }